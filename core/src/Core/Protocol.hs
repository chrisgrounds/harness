module Core.Protocol (receiveJSON, sendJSON) where

import Data.Aeson (FromJSON, Result (..), ToJSON, encode, fromJSON)
import Data.Aeson.Parser (json')
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

sendJSON :: (ToJSON a) => Socket -> a -> IO ()
sendJSON sock = sendAll sock . L.toStrict . encode

receiveJSON :: (FromJSON a) => Socket -> B.ByteString -> IO (Maybe (a, B.ByteString))
receiveJSON sock initial
  | B.null pending = do
      chunk <- recv sock 4096
      if B.null chunk then pure Nothing else receiveJSON sock chunk
  | otherwise = consume (A.parse json' pending)
  where
    pending = BC.dropWhile isSpace initial
    consume (A.Fail _ _ err) = ioError (userError ("Invalid socket JSON: " ++ err))
    consume (A.Done rest value) = case fromJSON value of
      Error err -> ioError (userError ("Invalid socket value: " ++ err))
      Success decoded -> pure (Just (decoded, rest))
    consume (A.Partial next) = do
      chunk <- recv sock 4096
      if B.null chunk
        then case next B.empty of
          done@(A.Done _ _) -> consume done
          _ -> ioError (userError "Socket disconnected before a complete JSON value arrived.")
        else consume (next chunk)
