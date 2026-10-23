{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAI
  ( Role (..),
    ToolRequest (..),
    ToolCallId,
    ToolRequestFunctionParams (..),
    ToolType (..),
    ToolName (..),
    OpenAIChatCompletionResponse (..),
    OpenAIChatResponseChoice (..),
    OpenAIChatResponseMessage (..),
    OpenAIChatResponseUsage (..),
    OpenAIChatCompletionTokensDetails (..),
    OpenAIChatPromptTokensDetails (..),
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Role = User | Assistant | System | Tool
  deriving (Show, Generic)

instance ToJSON Role where
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON System = String "system"
  toJSON Tool = String "tool"

instance FromJSON Role where
  parseJSON = withText "Role" $ \value ->
    case value of
      "user" -> pure User
      "assistant" -> pure Assistant
      "system" -> pure System
      "tool" -> pure Tool
      _ -> fail "unknown role"

data ToolRequest = ToolRequest
  { toolRequestId :: ToolCallId,
    toolRequestType :: ToolType,
    toolRequestFunction :: ToolRequestFunctionParams
  }
  deriving (Show, Generic)

type ToolCallId = Text

data ToolRequestFunctionParams = ToolRequestFunctionParams
  { toolRequestFunctionParamsName :: ToolName,
    toolRequestFunctionParamsArguments :: Text
  }
  deriving (Show, Generic)

data ToolType = FunctionTool
  deriving (Show)

data ToolName = ReadFile
  deriving (Show)

instance FromJSON ToolName where
  parseJSON = withText "ToolName" $ \value ->
    case value of
      "ReadFile" -> pure ReadFile
      _ -> fail "unknown tool name"

instance ToJSON ToolName where
  toJSON ReadFile = String "ReadFile"

instance FromJSON ToolType where
  parseJSON = withText "ToolType" $ \value ->
    case value of
      "function" -> pure FunctionTool
      _ -> fail "unknown type"

instance ToJSON ToolType where
  toJSON FunctionTool = String "function"

instance FromJSON ToolRequest where
  parseJSON = genericParseJSON (jsonOptions "toolRequest")

instance ToJSON ToolRequest where
  toJSON = genericToJSON (jsonOptions "toolRequest")

instance FromJSON ToolRequestFunctionParams where
  parseJSON = genericParseJSON (jsonOptions "toolRequestFunctionParams")

instance ToJSON ToolRequestFunctionParams where
  toJSON = genericToJSON (jsonOptions "toolRequestFunctionParams")

data OpenAIChatCompletionResponse = OpenAIChatCompletionResponse
  { responseChoices :: [OpenAIChatResponseChoice],
    responseCreated :: Integer,
    responseId :: Text,
    responseModel :: Text,
    responseObject :: Text,
    responseReasoningSummary :: Maybe Value,
    responseUsage :: OpenAIChatResponseUsage,
    responseWarning :: Maybe Value
  }
  deriving (Show, Generic)

data OpenAIChatResponseChoice = OpenAIChatResponseChoice
  { choiceFinishReason :: Maybe Text,
    choiceIndex :: Int,
    choiceMessage :: OpenAIChatResponseMessage
  }
  deriving (Show, Generic)

data OpenAIChatResponseMessage = OpenAIChatResponseMessage
  { messageContent :: Maybe Text,
    messageRole :: Role,
    messageToolCallId :: Maybe Text,
    messageToolCalls :: Maybe [ToolRequest]
  }
  deriving (Show, Generic)

data OpenAIChatResponseUsage = OpenAIChatResponseUsage
  { usageCompletionTokens :: Int,
    usageCompletionTokensDetails :: OpenAIChatCompletionTokensDetails,
    usagePromptTokens :: Int,
    usagePromptTokensDetails :: OpenAIChatPromptTokensDetails,
    usageTotalTokens :: Int
  }
  deriving (Show, Generic)

data OpenAIChatCompletionTokensDetails = OpenAIChatCompletionTokensDetails
  { completionReasoningTokens :: Int
  }
  deriving (Show, Generic)

data OpenAIChatPromptTokensDetails = OpenAIChatPromptTokensDetails
  { promptCachedTokens :: Int
  }
  deriving (Show, Generic)

jsonOptions :: String -> Options
jsonOptions prefix =
  defaultOptions
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
