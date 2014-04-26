{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module FileSystem where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar,
                                            takeMVar)
import           Control.Exception         (IOException, try)
import           Control.Monad             (forever, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Maybe
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as S
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.ByteString.Char8     as BC
import           Data.Time                 (getZonedTime, ZonedTime)
import           Data.Conduit              (MonadResource, Source, bracketP,
                                            runResourceT, ($$), ($=), yield)
import           Data.Conduit.Binary       (sourceFileRange, sinkIOHandle)
import qualified Data.Conduit.List         as CL
import           Data.IORef                (IORef, modifyIORef, newIORef,
                                            readIORef)
import           Filesystem                (canonicalizePath)
import           Filesystem.Path.CurrentOS (decodeString, directory)
import           System.FSNotify           (Event (..), startManager,
                                            stopManager, watchDir)
import           Control.Monad.Reader
import           Network.Xmpp
import           Network.Xmpp.IM
import           System.IO
import qualified System.Directory          as SD
import           DBus.Notify
import           DBus.Client

import           Models
import           Utils


tryIO :: IO a -> IO (Either IOException a)
tryIO = try


sendNotification contact message = do
    client <- connectSession
    let startNote = blankNote {
        appName="xi",
        summary=(T.unpack $ jidToText $ contactJid contact),
        body=(Just $ Text $ T.unpack message)
    }
    notification <- notify client startNote
    return ()


printMsg contact [] = return ()
printMsg contact (m:msgs) = do
    let content = TE.encodeUtf8(bodyContent m)
    let file = outputName contact
    localTime <- getZonedTime
    runResourceT $ yield (prettify contact localTime content) $$ sinkIOHandle $ openFile file AppendMode
    sendNotification contact (bodyContent m)
    printMsg contact msgs


sourceFileOutputForever sess contactList = forever $ do
    msg <- getMessage sess
    case (messageFrom msg) of
        Just value -> do
            let getByJid = \c -> (contactJid c) == (toBare value)
            let contact = head $ filter getByJid contactList
            printMsg contact $ imBody $ fromJust $ getIM msg
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


createFiles :: ContactList -> IO ()
createFiles [] = return ()
createFiles (c:contacts) = do
    let filePath = (T.unpack $ jidToText $ contactJid c) :: FilePath
    let inFilePath = filePath ++ "/in"
    let outFilePath = filePath ++ "/out"

    createContactDirectory filePath

    createContactFile inFilePath
    runResourceT $ yield "" $$ sinkIOHandle $ openFile inFilePath WriteMode

    createContactFile outFilePath

    createFiles contacts

    where
        createContactDirectory :: FilePath -> IO ()
        createContactDirectory filePath = do
            dirExist <- SD.doesDirectoryExist filePath
            if not dirExist
                then SD.createDirectory $ filePath
                else return ()

        createContactFile :: FilePath -> IO ()
        createContactFile filePath = do
            fileExist <- SD.doesFileExist filePath
            if not fileExist
                then do 
                    file <- openFile filePath WriteMode
                    hClose file
                    return ()

                else return ()
