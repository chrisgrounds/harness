{-# LANGUAGE OverloadedStrings #-}

module Client (receiveEvent, receiveGreeting, worker, NetworkEvent (..), safeText, stepChanges, statusIcon) where

import Control.Concurrent (Chan, readChan)
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
  | Tick
  | -- | The server announced the model it runs with.
    ServerModel Model
  | -- | The model sent a reply at this many tokens per second.
    ReplyRate Double

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

-- | Publishes the model from the server's greeting, then sends each
-- requested run and publishes its events until the turn finishes. A lost
-- connection is published as 'Disconnected'.
worker :: Socket -> Chan StartRun -> (NetworkEvent -> IO ()) -> IO ()
worker sock requests publish = do
  result <- try (receiveGreeting sock >>= \(m, rest) -> publish (ServerModel m) >> loop rest) :: IO (Either IOException ())
  case result of
    Left err -> publish (Disconnected (T.pack (displayException err)))
    Right () -> pure ()
  where
    loop pending = do
      req <- readChan requests
      sendJSON sock req
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

-- | Steps whose status differs between two snapshots, ignoring steps reset to
-- 'Pending' by a new iteration.
stepChanges :: Maybe Run -> Run -> [(Step, StepStatus)]
stepChanges previous run =
  [ change
  | change@(step, status) <- steps run,
    status /= Pending,
    maybe True (\p -> lookup step (steps p) /= Just status) previous
  ]

statusIcon :: StepStatus -> Text
statusIcon Pending = "○"
statusIcon Running = "●"
statusIcon Passed = "✓"
statusIcon (Failed _) = "✗"

safeText :: Text -> Text
safeText = T.replace "\t" "    " . T.filter (\c -> not (isControl c) || c == '\n' || c == '\t')
