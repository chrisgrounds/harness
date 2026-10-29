{-# LANGUAGE ScopedTypeVariables #-}

module Socket (serve) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (withAsync, link)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception hiding (Handler, handle)
import Control.Monad (forever, void, when)
import Data.Maybe (isNothing)
import Core hiding (model)
import Core.Protocol (receiveJSON, sendJSON)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import System.Directory (removeFile)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

type Handler = Model -> Maybe (IO Text) -> (ServerEvent -> IO ()) -> StartRun -> IO ()

-- Runs are serialized because they can edit the same working tree. Socket
-- reads remain live while a run waits for client-owned credential refreshes.
serve :: FilePath -> Model -> [Model] -> Handler -> IO ()
serve path model available handle = do
  projectLock <- newMVar ()
  bracket open closeListener $ \listener -> forever $ mask $ \restore -> do
    (conn, _) <- restore (accept listener)
    void $ forkFinally (restore (talk conn projectLock)) $ \result -> do
      _ <- try (close conn) :: IO (Either IOException ())
      case result of
        Left err -> hPutStrLn stderr ("client disconnected: " ++ displayException err)
        Right () -> pure ()
  where
    open = do
      listener <- socket AF_UNIX Stream defaultProtocol
      (bind listener (SockAddrUnix path) >> listen listener 16 >> pure listener) `onException` close listener
    closeListener listener = close listener `finally` removeFile path
    talk conn projectLock = do
      writeLock <- newMVar ()
      pendingTokens <- newMVar Map.empty
      serial <- newIORef (0 :: Int)
      jobs <- newTQueueIO
      let publish event = withMVar writeLock (\() -> sendJSON conn event)
          token account = do
            requestId <- atomicModifyIORef' serial (\i -> (i + 1, i))
            reply <- newEmptyMVar
            let register = modifyMVar_ pendingTokens (pure . Map.insert requestId reply)
                remove = modifyMVar_ pendingTokens (pure . Map.delete requestId)
            bracket_ register remove $ do
              publish (TokenNeeded requestId account)
              answer <- timeout (60 * 1000000) (takeMVar reply)
              either (fail . T.unpack) pure (maybe (Left (T.pack "Client did not supply OpenAI credentials.")) id answer)
          runJob message = withMVar projectLock $ \() -> case message of
            LegacyRun run -> handle model Nothing publish run `finally` publish TurnFinished
            StartChat chat chosen account run -> do
              let source = token <$> account
                  notify = publish . ChatEvent chat
              let execute = do
                    when (provider chosen == OpenAI && isNothing account) (fail "Choose a connected ChatGPT account in User settings.")
                    handle chosen source notify run
              (execute `catch` \(err :: IOException) -> publish (ChatFailed chat (T.pack (displayException err))))
                `finally` notify TurnFinished
            _ -> pure ()
          readRequests pending = do
            incoming <- receiveJSON conn pending
            case incoming of
              Nothing -> pure ()
              Just (message, rest) -> do
                case message of
                  ClientHello -> publish (AvailableModels available)
                  TokenResponse requestId result -> do
                    callbacks <- readMVar pendingTokens
                    mapM_ (\reply -> void (tryPutMVar reply result)) (Map.lookup requestId callbacks)
                  _ -> atomically (writeTQueue jobs message)
                readRequests rest
      publish (Connected model)
      withAsync (forever (atomically (readTQueue jobs) >>= runJob)) $ \worker -> do
        link worker
        readRequests B.empty
