{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Model (Model (..), Provider (..), allModels, provider, modelId, modelName, implementers, defaultImplementer, largerImplementer) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Claude models are ordered by capability: Haiku < Sonnet < Opus.
-- OpenAI slugs have no capability ranking.
data Model
  = Mercury3
  | -- | Claude Haiku 5.5.
    ClaudeHaiku
  | -- | Claude Sonnet 5.5.
    ClaudeSonnet
  | -- | Claude Opus 5.5.
    ClaudeOpus
  | -- | A model slug from the signed-in ChatGPT account's catalog.
    OpenAIModel Text
  deriving (Eq, Ord, Show, Generic)

data Provider = Inception | Anthropic | OpenAI
  deriving (Eq, Show)

provider :: Model -> Provider
provider Mercury3 = Inception
provider (OpenAIModel _) = OpenAI
provider _ = Anthropic

-- | Built-in models. OpenAI models are discovered from the account catalog.
allModels :: [Model]
allModels = [Mercury3, ClaudeHaiku, ClaudeSonnet, ClaudeOpus]

-- | The model's name in its provider's API.
modelId :: Model -> Text
modelId (OpenAIModel slug) = slug
modelId Mercury3 = "mercury-3"
modelId ClaudeOpus = "claude-opus-5-5"
modelId ClaudeSonnet = "claude-sonnet-5-5"
modelId ClaudeHaiku = "claude-haiku-5-5"

-- | The model's short name, as @--model@ and the design take it.
modelName :: Model -> Text
modelName (OpenAIModel slug) = "openai:" <> slug
modelName Mercury3 = "mercury"
modelName ClaudeOpus = "opus"
modelName ClaudeSonnet = "sonnet"
modelName ClaudeHaiku = "haiku"

-- | The models a run on the given model implements functions with,
-- cheapest first: its provider's models up to its own.
implementers :: Model -> [Model]
implementers m@(OpenAIModel _) = [m]
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
  toJSON m@(OpenAIModel _) = String (modelName m)
  toJSON m = String (modelId m)

instance FromJSON Model where
  parseJSON = withText "Model" $ \m ->
    case [model | model <- allModels, modelId model == m] of
      model : _ -> pure model
      [] -> case T.stripPrefix "openai:" m of
        Just slug | not (T.null slug) -> pure (OpenAIModel slug)
        _ -> fail "unknown model"
