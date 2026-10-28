module Socket (serve, talk) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (IOException, bracket, finally, mask, onException, try)
import Control.Monad (forever, void)
import Core (ServerEvent (..), StartRun)
import Core.Protocol (receiveJSON, sendJSON)
import qualified Data.ByteString as B
import Network.Socket
import System.Directory (removeFile)
import System.IO (hPutStrLn, stderr)
import Core (Model)

-- | Each request is handed to the handler with a way to publish events
-- for it; 'TurnFinished' is sent once the handler returns.
type Handler = (ServerEvent -> IO ()) -> StartRun -> IO ()

-- | As before, with a new Model argument: the model the server runs with, which is passed to `talk` for every accepted connection so each client is greeted with it. (The server's main must pass the model selected by --model, i.e. the one given to `holey`.)
serve :: FilePath -> Model -> Handler -> IO ()
serve path model handle = bracket open closeListener $ \listener ->
  forever $ mask $ \restore -> do
    (conn, _) <- restore (accept listener)
    void $ forkFinally (restore (talk conn model handle)) $ \result -> do
      _ <- try (close conn) :: IO (Either IOException ())
      case result of
        Left err -> hPutStrLn stderr ("client disconnected: " ++ show err)
        Right () -> pure ()
  where
    open = do
      listener <- socket AF_UNIX Stream defaultProtocol
      (bind listener (SockAddrUnix path) >> listen listener 16 >> pure listener)
        `onException` close listener
    closeListener listener = close listener `finally` removeFile path

-- | As before, but immediately after creating the publish lock and before reading any request it publishes `Connected model` (through the same locked publish function), so the greeting is the first value the client receives. Then the request loop runs unchanged: each request is handled and followed by 'TurnFinished'.
talk :: Socket -> Model -> Handler -> IO ()
talk conn model handle = do
  -- Handlers may publish from several threads at once.
  lock <- newMVar ()
  let publish event = withMVar lock (\() -> sendJSON conn event)
      loop pending = do
        incoming <- receiveJSON conn pending
        case incoming of
          Nothing -> pure ()
          Just (req, rest) -> do
            handle publish req
            publish TurnFinished
            loop rest
  publish (Connected model)
  loop B.empty
