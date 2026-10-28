{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Model (Model (..), allModels, modelId, modelName, implementers, defaultImplementer, largerImplementer) where

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

-- | The model's short name, as @--model@ and the design take it.
modelName :: Model -> Text
modelName Mercury3 = "mercury"
modelName ClaudeOpus = "opus"
modelName ClaudeSonnet = "sonnet"
modelName ClaudeHaiku = "haiku"

-- | The models a run on the given model implements functions with,
-- cheapest first: the Claude models up to its own, or Mercury alone, the
-- only Inception model.
implementers :: Model -> [Model]
implementers Mercury3 = [Mercury3]
implementers ClaudeOpus = [ClaudeHaiku, ClaudeSonnet, ClaudeOpus]
implementers ClaudeSonnet = [ClaudeHaiku, ClaudeSonnet]
implementers ClaudeHaiku = [ClaudeHaiku]

-- | The implementer when the design does not choose one: the next cheaper
-- model than the run's, if there is one.
defaultImplementer :: Model -> Model
defaultImplementer m = case reverse (implementers m) of
  _ : cheaper : _ -> cheaper
  _ -> m

-- | The next larger implementer than the given one, for a function whose
-- implementation failed.
largerImplementer :: Model -> Model -> Model
largerImplementer run m = case dropWhile (/= m) (implementers run) of
  _ : larger : _ -> larger
  _ -> m

instance ToJSON Model where
  toJSON = String . modelId

instance FromJSON Model where
  parseJSON = withText "Model" $ \m ->
    case [model | model <- allModels, modelId model == m] of
      model : _ -> pure model
      [] -> fail "unknown model"
