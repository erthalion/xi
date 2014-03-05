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
import qualified Data.Text.Encoding        as T
import           Data.Conduit              (MonadResource, Source, bracketP,
                                            runResourceT, ($$), ($=))
import           Data.Conduit.Binary       (sourceFileRange)
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
import           Network.TLS               (Params(pConnectVersion, pAllowedVersions, pCiphers), 
                                            Version(TLS10, TLS11, TLS12), defaultParamsClient)
import           Network.TLS.Extra         (ciphersuite_medium)


data Configuration = Configuration {
    clientSession :: Session
}

type XIClient = ReaderT Configuration IO

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

{-sourceFileOutputForever sess = forever $ do-}
    {-msg <- getMessage sess-}
    {-case answerMessage msg (messagePayload msg) of-}
        {-Just answer -> putStrLn answer >> return ()-}
        {-Nothing -> putStrLn "Received message with no sender."-}


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

main :: IO ()
main = do
    let inFiles = ["in1", "in2"]
    let outFiles = ["out1", "out2"]
    let identifiers = ["9erthalion.war6@gmail.com", "test2"]

    sess <- establishConnection

    listenIn sess (zip inFiles identifiers)
    listenOut sess (zip outFiles identifiers)
    forever $ threadDelay (10^6) 
  where
    listenIn :: Session -> [(String, String)] -> IO ()
    listenIn _ [] = return ()
    listenIn sess (channel:channels) = do
        let identifier = snd channel
        let file = fst channel
        let handleWithIdentifier = handleCommand sess identifier
        _ <- forkIO $ runResourceT $ sourceFileForever file $$ CL.mapM_ (liftIO . handleWithIdentifier)
        listenIn sess channels

    listenOut :: Session -> [(String, String)] -> IO ()
    listenOut _ [] = return ()
    listenOut sess (channel:channels) = do
        let identifier = snd channel
        let file = fst channel

        {-_ <- forkIO $  sourceFileOutputForever sess-}
        listenOut sess channels

    establishConnection :: IO Session 
    establishConnection = do
        updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
        result <- session
                     "gmail.com"
                      (Just (\_ -> ( [plain "user" Nothing "pass"])
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


handleCommand :: Session -> String -> ByteString -> IO()
handleCommand sess identifier message = do
    let contactJid = parseJid identifier
    sendMsg sess message contactJid


sendMsg :: Session -> ByteString -> Jid -> IO()
sendMsg sess message contactJid = do
    let messageText = T.decodeUtf8 message
    let msgC = simpleIM contactJid messageText
    void $ sendMessage msgC sess
