module Socket (serve, talk) where

import Control.Concurrent (forkFinally)
import Control.Exception (IOException, bracket, finally, mask, onException, try)
import Control.Monad (forever, void)
import Core (Chat, Message)
import Core.Protocol (receiveJSON, sendJSON)
import qualified Data.ByteString as B
import Network.Socket
import System.Directory (removeFile)
import System.IO (hPutStrLn, stderr)

serve :: FilePath -> Chat -> (Chat -> Message -> IO Chat) -> IO ()
serve path initial handleMessage = bracket open closeListener $ \listener ->
  forever $ mask $ \restore -> do
    (conn, _) <- restore (accept listener)
    void $ forkFinally (restore (talk conn initial handleMessage)) $ \result -> do
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

talk :: Socket -> Chat -> (Chat -> Message -> IO Chat) -> IO ()
talk conn initial handleMessage = loop B.empty initial
  where
    loop pending chat = do
      incoming <- receiveJSON conn pending
      case incoming of
        Nothing -> pure ()
        Just (message, rest) -> do
          updated <- handleMessage chat message
          sendJSON conn updated
          loop rest updated
