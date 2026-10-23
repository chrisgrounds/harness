{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OpenAI
  ( Role (User, Agent, System)
  , OpenAIChatCompletionResponse (..)
  , OpenAIChatResponseChoice (..)
  , OpenAIChatResponseMessage (..)
  , OpenAIChatResponseUsage (..)
  , OpenAIChatCompletionTokensDetails (..)
  , OpenAIChatPromptTokensDetails (..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Role = User | Agent | System
  deriving (Show, Generic)

instance ToJSON Role where
  toJSON User   = String "user"
  toJSON Agent  = String "assistant"
  toJSON System = String "system"

instance FromJSON Role where
  parseJSON = withText "Role" $ \value ->
    case value of
      "user"      -> pure User
      "assistant" -> pure Agent
      "system"    -> pure System
      _           -> fail "unknown role"

data OpenAIChatCompletionResponse = OpenAIChatCompletionResponse
  { responseChoices          :: [OpenAIChatResponseChoice]
  , responseCreated          :: Integer
  , responseId               :: Text
  , responseModel            :: Text
  , responseObject           :: Text
  , responseReasoningSummary :: Maybe Value
  , responseUsage            :: OpenAIChatResponseUsage
  , responseWarning          :: Maybe Value
  } deriving (Show, Generic)

data OpenAIChatResponseChoice = OpenAIChatResponseChoice
  { choiceFinishReason :: Maybe Text
  , choiceIndex        :: Int
  , choiceMessage      :: OpenAIChatResponseMessage
  } deriving (Show, Generic)

data OpenAIChatResponseMessage = OpenAIChatResponseMessage
  { messageContent    :: Maybe Text
  , messageRole       :: Role
  , messageToolCallId :: Maybe Text
  , messageToolCalls  :: Maybe [Value]
  } deriving (Show, Generic)

data OpenAIChatResponseUsage = OpenAIChatResponseUsage
  { usageCompletionTokens        :: Int
  , usageCompletionTokensDetails :: OpenAIChatCompletionTokensDetails
  , usagePromptTokens            :: Int
  , usagePromptTokensDetails     :: OpenAIChatPromptTokensDetails
  , usageTotalTokens             :: Int
  } deriving (Show, Generic)

data OpenAIChatCompletionTokensDetails = OpenAIChatCompletionTokensDetails
  { completionReasoningTokens :: Int
  } deriving (Show, Generic)

data OpenAIChatPromptTokensDetails = OpenAIChatPromptTokensDetails
  { promptCachedTokens :: Int
  } deriving (Show, Generic)

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier =
      camelTo2 '_' . Prelude.drop (Prelude.length prefix)
  }

instance FromJSON OpenAIChatCompletionResponse where
  parseJSON = genericParseJSON (jsonOptions "response")

instance FromJSON OpenAIChatResponseChoice where
  parseJSON = genericParseJSON (jsonOptions "choice")

instance FromJSON OpenAIChatResponseMessage where
  parseJSON = genericParseJSON (jsonOptions "message")

instance FromJSON OpenAIChatResponseUsage where
  parseJSON = genericParseJSON (jsonOptions "usage")

instance FromJSON OpenAIChatCompletionTokensDetails where
  parseJSON = genericParseJSON (jsonOptions "completion")

instance FromJSON OpenAIChatPromptTokensDetails where
  parseJSON = genericParseJSON (jsonOptions "prompt")
