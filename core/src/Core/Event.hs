{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Event (StartRun (..), ClientMessage (..), ServerEvent (..)) where

import Core.Run (Requirement, Run)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Core.Model (Model, ReasoningEffort)

-- | What the client sends: a requirement to run against a cabal project.
data StartRun = StartRun
  { -- | Absolute, since the server's working directory is unrelated.
    runProject :: FilePath,
    runRequirement :: Requirement,
    -- | Omitted by older clients; Nothing uses the model's default.
    runReasoningEffort :: Maybe ReasoningEffort
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | On each new connection the server first sends one 'Connected' naming the
-- default for legacy clients. GUI clients send chat-local model choices;
-- their run events are wrapped in 'ChatEvent'. It replies to each 'StartRun' with any number of
-- 'RunUpdated' snapshots of that requirement's run, and a 'TokenRate' for
-- each reply the model sends, then one 'TurnFinished'.
data ServerEvent
  = Connected Model
  | AvailableModels [Model]
  | ChatEvent Int ServerEvent
  | TokenNeeded Int Text
  | ChatFailed Int Text
  | RunUpdated Run
  | TurnFinished
  | -- | The model sent a reply at this many tokens per second.
    TokenRate Double
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | GUI requests carry chat-local model/account choices. Legacy clients can
-- continue sending a bare StartRun. Access tokens are never shown in logs.
data ClientMessage
  = ClientHello
  | LegacyRun StartRun
  | StartChat Int Model (Maybe Text) StartRun
  | TokenResponse Int (Either Text Text)
  deriving (Eq, Generic)

instance Show ClientMessage where
  show ClientHello = "ClientHello"
  show (LegacyRun run) = "LegacyRun " ++ show run
  show (StartChat chat model account run) = "StartChat " ++ show (chat, model, account, run)
  show (TokenResponse request _) = "TokenResponse " ++ show request ++ " <redacted>"

instance ToJSON ClientMessage where
  toJSON (LegacyRun run) = toJSON run
  toJSON message = genericToJSON defaultOptions message
instance FromJSON ClientMessage where
  parseJSON value = genericParseJSON defaultOptions value <|> (LegacyRun <$> parseJSON value)
