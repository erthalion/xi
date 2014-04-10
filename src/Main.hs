{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Exception         (IOException, try)
import           Control.Monad             (forever, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Maybe
import           Data.ByteString           (ByteString)
import qualified Data.Text.Encoding        as TE
import qualified Data.Text                 as T
import qualified Data.Yaml.Config          as Y
import qualified Data.Map                  as M
import           Data.Time                 (getZonedTime, ZonedTime)
import           Data.Conduit              (MonadResource, Source, bracketP,
                                            runResourceT, ($$), ($=), yield)
import           Data.Conduit.Binary       (sourceFileRange, sinkIOHandle)
import qualified Data.Conduit.List         as CL
import           Control.Monad.Reader
import           Network.Xmpp
import           Network.Xmpp.IM
import           System.Log.Logger
import           System.IO
import           Network.TLS               (Params(pConnectVersion, pAllowedVersions, pCiphers), 
                                            Version(TLS10, TLS11, TLS12), defaultParamsClient)
import           Network.TLS.Extra         (ciphersuite_medium)

import           Models
import           FileSystem
import           Utils


inFilePath :: FilePath
inFilePath = "in"


outFilePath :: FilePath
outFilePath = "out"


main :: IO ()
main = do
    config <- Y.load "xi.yml"
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
    localTime <- getZonedTime
    runResourceT $ yield (prettify contact localTime message) $$ sinkIOHandle $ openFile (outputName contact) AppendMode


sendMsg :: Session -> ByteString -> Jid -> IO()
sendMsg sess message contactJid = do
    let messageText = TE.decodeUtf8 message
    let msgC = simpleIM contactJid messageText
    void $ sendMessage msgC sess
