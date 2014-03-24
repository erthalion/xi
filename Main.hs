{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar,
                                            takeMVar)
import           Control.Exception         (IOException, try)
import           Control.Monad             (forever, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Maybe
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as S
import qualified Data.Text.Encoding        as TE
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TO
import qualified Data.ByteString.Char8     as BC
import qualified Data.Yaml.Config          as Y
import qualified Data.Map                  as M
import           Data.Conduit              (MonadResource, Source, bracketP,
                                            runResourceT, ($$), ($=), yield)
import           Data.Conduit.Binary       (sourceFileRange, sinkIOHandle)
import qualified Data.Conduit.List         as CL
import           Data.IORef                (IORef, modifyIORef, newIORef,
                                            readIORef)
import           Data.Time                 (getCurrentTime)
import           Filesystem                (canonicalizePath)
import           Filesystem.Path.CurrentOS (decodeString, directory)
import           System.FSNotify           (Event (..), startManager,
                                            stopManager, watchDir)
import           Control.Monad.Reader
import           Network.Xmpp
import           Network.Xmpp.IM
import           System.Log.Logger
import           System.IO
import qualified System.Directory          as SD
import           Network.TLS               (Params(pConnectVersion, pAllowedVersions, pCiphers), 
                                            Version(TLS10, TLS11, TLS12), defaultParamsClient)
import           Network.TLS.Extra         (ciphersuite_medium)


data Configuration = Configuration {
    clientSession :: Session,
    contactList :: ContactList
}

type ContactList = [Contact]

data Contact = Contact {
    contactJid :: Jid,
    name :: T.Text,
    inputName :: String,
    outputName :: String
} deriving (Show)

type XIConfig a = ReaderT Configuration IO a

tryIO :: IO a -> IO (Either IOException a)
tryIO = try


printMsg file [] = return ()
printMsg file (m:msgs) = do
    let content = BC.pack $ T.unpack (bodyContent m) ++ "\n"
    runResourceT $ yield content $$ sinkIOHandle $ openFile file AppendMode
    printMsg file msgs


sourceFileOutputForever sess contactList = forever $ do
    msg <- getMessage sess
    case (messageFrom msg) of
        Just value -> do
            let getByJid = \c -> (contactJid c) == (toBare value)
            let contact = head $ filter getByJid contactList
            liftIO $ print contact
            printMsg (outputName contact) $ imBody $ fromJust $ getIM msg
        Nothing -> return ()


sourceFileForever :: MonadResource m => FilePath -> Source m ByteString
sourceFileForever fp' = bracketP startManager stopManager $ \manager -> do
    fp <- liftIO $ canonicalizePath $ decodeString fp'
    baton <- liftIO newEmptyMVar
    liftIO $ watchDir manager (directory fp) (const True) $ \event -> void $ tryIO $ do
        fpE <- canonicalizePath $
            case event of
                Added x _ -> x
                Modified x _ -> x
                Removed x _ -> x
        when (fpE == fp) $ putMVar baton ()
    consumedRef <- liftIO $ newIORef 0
    loop baton consumedRef
  where
    loop :: MonadResource m => MVar () -> IORef Integer -> Source m ByteString
    loop baton consumedRef = forever $ do
        consumed <- liftIO $ readIORef consumedRef
        sourceFileRange fp' (Just consumed) Nothing $= CL.iterM counter
        liftIO $ takeMVar baton
      where
        counter bs = liftIO $ modifyIORef consumedRef (+ fromIntegral (S.length bs))


inFilePath :: FilePath
inFilePath = "in"

outFilePath :: FilePath
outFilePath = "out"

main :: IO ()
main = do
    config <- Y.load "xi.yml"

    inHFile <- openFile inFilePath AppendMode
    outHFile <- openFile outFilePath AppendMode

    sess <- establishConnection config

    roster <- getRoster sess
    let contactList = map convert (M.elems $ items roster)
    createFiles contactList

    runReaderT listen (Configuration sess contactList)
    forever $ threadDelay (10^6) 
  where
    convert :: Item -> Contact
    convert item = do
        let filePath = (T.unpack $ jidToText $ riJid item) :: FilePath
        let inFilePath = filePath ++ "/in"
        let outFilePath = filePath ++ "/out"

        Contact {
            contactJid=(riJid item),
            name=(jidToText $ riJid item),
            inputName=inFilePath,
            outputName=outFilePath
        }

    createFiles :: ContactList -> IO ()
    createFiles [] = return ()
    createFiles (c:contacts) = do
        let filePath = (T.unpack $ jidToText $ contactJid c) :: FilePath
        let inFilePath = filePath ++ "/in"
        let outFilePath = filePath ++ "/out"

        SD.createDirectory $ filePath
        openFile inFilePath WriteMode
        openFile outFilePath WriteMode

        createFiles contacts


    listen :: XIConfig ()
    listen = do
        conf <- ask
        listenIn $ contactList conf
        listenOut

    listenIn :: ContactList -> XIConfig ()
    listenIn [] = return ()
    listenIn (c:contacts) = do
        conf <- ask
        let handleWithContact = handleCommand (clientSession conf) c
        _ <- liftIO $ forkIO $ runResourceT $ sourceFileForever (inputName c) $$ CL.mapM_ (liftIO . handleWithContact)
        listenIn contacts

    listenOut :: XIConfig ()
    listenOut = do
        conf <- ask
        _ <- liftIO $ forkIO $ sourceFileOutputForever (clientSession conf) (contactList conf)
        return ()

    establishConnection :: Y.Config -> IO Session 
    establishConnection config = do
        connection <- Y.subconfig "Connection" config
        client <- Y.subconfig "Client" config

        debug <- Y.lookup "debug" client
        server <- Y.lookup "server" connection
        user <- Y.lookup "user" connection
        password <- Y.lookup "password" connection

        when debug $ updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

        result <- session
                     server
                      (Just (\_ -> ( [plain user Nothing password])
                                   , Nothing))
                    def { sessionStreamConfiguration = def
                            { tlsParams = defaultParamsClient
                                { pConnectVersion = TLS10
                                , pAllowedVersions = [TLS10, TLS11, TLS12]
                                , pCiphers = ciphersuite_medium } } }
        sess <- case result of
                    Right s -> return s
                    Left e -> error $ "XmppFailure: " ++ (show e)
        sendPresence def sess
        return sess


handleCommand :: Session -> Contact -> ByteString -> IO()
handleCommand sess contact message = do
    sendMsg sess message (contactJid contact)
    runResourceT $ yield message $$ sinkIOHandle $ openFile (outputName contact) AppendMode


sendMsg :: Session -> ByteString -> Jid -> IO()
sendMsg sess message contactJid = do
    let messageText = TE.decodeUtf8 message
    let msgC = simpleIM contactJid messageText
    void $ sendMessage msgC sess
