{-# LANGUAGE OverloadedStrings #-}

module BendCompatibilityLayer (serve)
where

import Chat (Chat, Message)
import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Exception (IOException, bracket, finally, mask, onException, try)
import Control.Monad (forever, void)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.IO (hPutStrLn, stderr)

-- Both listeners are local. Binding TCP never exposes Harness on the LAN.
-- The Unix listener retains the existing wire protocol for older clients.
withListeners :: FilePath -> PortNumber -> (Socket -> Socket -> IO a) -> IO a
withListeners path port action =
  bracket openUnix closeUnix $ \unix ->
    bracket openTcp close $ \tcp -> action unix tcp
  where
    openUnix = do
      s <- socket AF_UNIX Stream defaultProtocol
      (bind s (SockAddrUnix path) >> listen s 16 >> pure s) `onException` close s
    closeUnix s = close s `finally` removeFile path
    openTcp = do
      s <- socket AF_INET Stream defaultProtocol
      ( do
          setSocketOption s ReuseAddr 1
          bind s (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
          listen s 16
          pure s
        ) `onException` close s

-- A disconnected or malformed client must not bring down either listener.
serveConnections :: Socket -> (Socket -> IO ()) -> IO ()
serveConnections listener handler = forever $ mask $ \restore -> do
  (conn, _) <- restore (accept listener)
  void $ forkFinally (restore (handler conn)) $ \result -> do
    _ <- try (close conn) :: IO (Either IOException ())
    case result of
      Left err -> hPutStrLn stderr ("client disconnected: " ++ show err)
      Right () -> pure ()

-- TCP uses LF-delimited JSON, with at most 1024 payload bytes per request.
-- Keep any following request bytes for the next turn; recv boundaries are not
-- message boundaries. A truncated final frame is an error, not a valid request.
receiveLine :: Socket -> B.ByteString -> IO (Maybe (B.ByteString, B.ByteString))
receiveLine conn pending =
  case B.elemIndex 10 pending of
    Just index
      | index > 1024 -> tooLarge
      | otherwise -> pure (Just (B.take index pending, B.drop (index + 1) pending))
    Nothing
      | B.length pending > 1024 -> tooLarge
      | otherwise -> do
          chunk <- recv conn 4096
          if B.null chunk
            then
              if B.null pending
                then pure Nothing
                else ioError (userError "truncated TCP request")
            else receiveLine conn (pending <> chunk)
  where
    tooLarge = ioError (userError "TCP request exceeds 1024 JSON bytes")

-- Own the compatibility transport here; the server supplies its ordinary
-- message handler and has no knowledge of TCP framing or buffering.
serve :: FilePath -> Chat -> (Chat -> Message -> IO Chat) -> IO ()
serve path initial handleMessage =
  withListeners path 7777 $ \unix tcp ->
    bracket
      (forkIO (serveConnections tcp (\conn -> talk True conn B.empty initial handleMessage)))
      killThread
      (\_ -> serveConnections unix (\conn -> talk False conn B.empty initial handleMessage))

talk :: Bool -> Socket -> B.ByteString -> Chat -> (Chat -> Message -> IO Chat) -> IO ()
talk framed conn pending chat handleMessage = do
  incoming <-
    if framed
      then receiveLine conn pending
      else do
        bytes <- recv conn 1024
        pure $ if B.null bytes then Nothing else Just (bytes, B.empty)
  case incoming of
    Nothing -> pure ()
    Just (bytes, rest) -> do
      message <- either fail pure (eitherDecodeStrict bytes)
      updatedChat <- handleMessage chat message
      let payload = L.toStrict (encode updatedChat)
      sendAll conn (if framed then payload <> "\n" else payload)
      talk framed conn rest updatedChat handleMessage
