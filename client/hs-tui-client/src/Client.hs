{-# LANGUAGE OverloadedStrings #-}

module Client (receiveEvent, worker, NetworkEvent (..), safeText, stepChanges, statusIcon) where

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

receiveEvent :: Socket -> B.ByteString -> IO (ServerEvent, B.ByteString)
receiveEvent sock pending = do
  reply <- receiveJSON sock pending
  maybe (ioError (userError "Server disconnected before the run finished.")) pure reply

-- | As before, but before waiting for the first request it reads the server's greeting with `receiveEvent sock B.empty`: if it is `Connected m`, publish `ServerModel m` and continue the request loop with the leftover bytes; any other first event is a protocol error raised with `ioError (userError "Server did not announce its model.")` (caught like the other IOExceptions and published as 'Disconnected'). Inside the per-request event loop, a `Connected m` event publishes `ServerModel m` and keeps reading; 'RunUpdated' and 'TurnFinished' are handled as before.
worker :: Socket -> Chan StartRun -> (NetworkEvent -> IO ()) -> IO ()
worker sock requests publish = do
  result <- try (greet >>= loop) :: IO (Either IOException ())
  case result of
    Left err -> publish (Disconnected (T.pack (displayException err)))
    Right () -> pure ()
  where
    greet = do
      (event, rest) <- receiveEvent sock B.empty
      case event of
        Connected m -> publish (ServerModel m) >> pure rest
        _ -> ioError (userError "Server did not announce its model.")
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
