{-# LANGUAGE OverloadedStrings #-}

module Client (userMessage, receiveChat, worker, NetworkEvent (..), safeText) where

import Control.Concurrent (Chan, readChan)
import Control.Exception (IOException, displayException, try)
import Core
import Core.Protocol (receiveJSON, sendJSON)
import qualified Data.ByteString as B
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Socket)

data NetworkEvent = Received Chat | Disconnected Text | Tick

userMessage :: Text -> Message
userMessage value = Message User (Just value) [] Nothing Nothing

receiveChat :: Socket -> B.ByteString -> IO (Chat, B.ByteString)
receiveChat sock pending = do
  reply <- receiveJSON sock pending
  maybe (ioError (userError "Server disconnected before replying.")) pure reply

worker :: Socket -> Chan Message -> (NetworkEvent -> IO ()) -> IO ()
worker sock requests publish = do
  result <- try (loop B.empty) :: IO (Either IOException ())
  case result of
    Left err -> publish (Disconnected (T.pack (displayException err)))
    Right () -> pure ()
  where
    loop pending = do
      message <- readChan requests
      sendJSON sock message
      (reply, rest) <- receiveChat sock pending
      publish (Received reply)
      loop rest

safeText :: Text -> Text
safeText = T.replace "\t" "    " . T.filter (\c -> not (isControl c) || c == '\n' || c == '\t')
