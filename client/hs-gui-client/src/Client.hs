{-# LANGUAGE OverloadedStrings #-}

module Client (NetworkEvent (..), worker, safeText) where

import Control.Exception (IOException, displayException, try)
import Core
import Core.Protocol (receiveJSON, sendJSON)
import qualified Data.ByteString as B
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Socket)

data NetworkEvent
  = RunChanged Run
  | TurnDone
  | Disconnected Text
  | -- | The server announced the model it runs with.
    ServerModel Model
  | -- | The model sent a reply at this many tokens per second.
    ReplyRate Double
  deriving (Eq, Show)

receiveEvent :: Socket -> B.ByteString -> IO (ServerEvent, B.ByteString)
receiveEvent sock pending = do
  reply <- receiveJSON sock pending
  maybe (ioError (userError "Server disconnected before the run finished.")) pure reply

-- | The model the server greets each connection with, and the bytes after it.
receiveGreeting :: Socket -> IO (Model, B.ByteString)
receiveGreeting sock = do
  (event, rest) <- receiveEvent sock B.empty
  case event of
    Connected m -> pure (m, rest)
    _ -> ioError (userError "Server did not announce its model.")

-- | Publishes the model from the server's greeting, then sends each run
-- @next@ returns and publishes its events until the turn finishes. A lost
-- connection is published as 'Disconnected'.
worker :: Socket -> IO StartRun -> (NetworkEvent -> IO ()) -> IO ()
worker sock next publish = do
  result <- try (receiveGreeting sock >>= \(m, rest) -> publish (ServerModel m) >> loop rest) :: IO (Either IOException ())
  either (publish . Disconnected . T.pack . displayException) pure result
  where
    loop pending = do
      next >>= sendJSON sock
      rest <- events pending
      publish TurnDone
      loop rest
    events pending = do
      (event, rest) <- receiveEvent sock pending
      case event of
        Connected m -> publish (ServerModel m) >> events rest
        RunUpdated run -> publish (RunChanged run) >> events rest
        TokenRate r -> publish (ReplyRate r) >> events rest
        TurnFinished -> pure rest

-- | Text as it can be drawn: no control characters but newlines, and tabs
-- as four spaces.
safeText :: Text -> Text
safeText = T.replace "\t" "    " . T.filter (\c -> not (isControl c) || c == '\n' || c == '\t')
