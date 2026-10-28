{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Model (Model (..), Provider (..), allModels, provider, modelId, modelName, implementers, defaultImplementer, largerImplementer) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Ordered by capability, and so by cost, within each provider: Haiku <
-- Sonnet < Opus. Comparing models of different providers means nothing.
data Model
  = Mercury3
  | -- | Claude Haiku 5.5.
    ClaudeHaiku
  | -- | Claude Sonnet 5.5.
    ClaudeSonnet
  | -- | Claude Opus 5.5.
    ClaudeOpus
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data Provider = Inception | Anthropic
  deriving (Eq, Show)

provider :: Model -> Provider
provider Mercury3 = Inception
provider _ = Anthropic

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
-- cheapest first: its provider's models up to its own.
implementers :: Model -> [Model]
implementers m = [i | i <- allModels, provider i == provider m, i <= m]

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
