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

data NetworkEvent = RunChanged Run | TurnDone | Disconnected Text | Tick

receiveEvent :: Socket -> B.ByteString -> IO (ServerEvent, B.ByteString)
receiveEvent sock pending = do
  reply <- receiveJSON sock pending
  maybe (ioError (userError "Server disconnected before the run finished.")) pure reply

worker :: Socket -> Chan Requirement -> (NetworkEvent -> IO ()) -> IO ()
worker sock requests publish = do
  result <- try (loop B.empty) :: IO (Either IOException ())
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
