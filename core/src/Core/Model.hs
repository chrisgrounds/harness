{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Model (Model (..), allModels, modelId) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Model
  = Mercury3
  | -- | Claude Opus 5.5.
    ClaudeOpus
  | -- | Claude Sonnet 5.5.
    ClaudeSonnet
  | -- | Claude Haiku 5.5.
    ClaudeHaiku
  deriving (Eq, Show, Enum, Bounded, Generic)

allModels :: [Model]
allModels = [minBound .. maxBound]

-- | The model's name in its provider's API.
modelId :: Model -> Text
modelId Mercury3 = "mercury-3"
modelId ClaudeOpus = "claude-opus-5-5"
modelId ClaudeSonnet = "claude-sonnet-5-5"
modelId ClaudeHaiku = "claude-haiku-5-5"

instance ToJSON Model where
  toJSON = String . modelId

instance FromJSON Model where
  parseJSON = withText "Model" $ \m ->
    case [model | model <- allModels, modelId model == m] of
      model : _ -> pure model
      [] -> fail "unknown model"
