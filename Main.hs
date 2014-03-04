import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar,
                                            takeMVar)
import           Control.Exception         (IOException, try)
import           Control.Monad             (forever, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as S
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

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

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
    let files = ["in1", "in2"]
    let identifiers = ["test1", "test2"]
    let channels = zip files identifiers
    listen $ zip files identifiers
    forever $ threadDelay (10^6) 
  where
    listen :: [(String, String)] -> IO()
    listen [] = return ()
    listen (channel:channels) = do
        let identifier = snd channel
        let file = fst channel
        let handleWithIdentifier = handleCommand identifier
        _ <- forkIO $ runResourceT $ sourceFileForever file $$ CL.mapM_ (liftIO . handleWithIdentifier)
        listen channels

handleCommand :: String -> ByteString -> IO()
handleCommand identifier command = do
    print identifier
    print command
