{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Chat
  ( Chat (..),
    Message (..),
    Model (..),
    ToolDefinition (..),
    ToolFunction (..),
    addMessage,
    addMessages,
    getLatestToolRequests,
  )
where

import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import OpenAI (Role, ToolCallId, ToolName, ToolRequest, ToolType)

data Chat = Chat
  { messages :: [Message],
    model :: Model,
    tools :: [ToolDefinition]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message
  { role :: Role,
    content :: Maybe Text,
    toolCalls :: [ToolRequest],
    toolCallId :: Maybe ToolCallId
  }
  deriving (Show, Generic)

getLatestToolRequests :: Chat -> [ToolRequest]
getLatestToolRequests = maybe [] toolCalls . listToMaybe . messages

instance FromJSON Message where
  parseJSON = withObject "Message" $ \value ->
    Message
      <$> value .: "role"
      <*> value .:? "content"
      <*> value .:? "tool_calls" .!= []
      <*> value .:? "tool_call_id"

instance ToJSON Message where
  toJSON message =
    object $
      [ "role" .= role message,
        "content" .= content message
      ]
        ++ ["tool_calls" .= toolCalls message | not (null (toolCalls message))]
        ++ ["tool_call_id" .= callId | Just callId <- [toolCallId message]]

addMessage :: Message -> Chat -> Chat
addMessage m c = c {messages = m : messages c}

addMessages :: [Message] -> Chat -> Chat
addMessages ms c = c {messages = ms ++ messages c}

data ToolDefinition = ToolDefinition
  { toolType :: ToolType,
    toolFunction :: ToolFunction
  }
  deriving (Show, Generic)

data ToolFunction = ToolFunction
  { toolFunctionName :: ToolName,
    toolFunctionDescription :: Text,
    toolFunctionParameters :: Value
  }
  deriving (Show, Generic)

jsonOptions :: String -> Options
jsonOptions prefix =
  defaultOptions
    { fieldLabelModifier =
        camelTo2 '_' . Prelude.drop (Prelude.length prefix)
    }

instance FromJSON ToolDefinition where
  parseJSON = genericParseJSON (jsonOptions "tool")

instance ToJSON ToolDefinition where
  toJSON = genericToJSON (jsonOptions "tool")

instance FromJSON ToolFunction where
  parseJSON = genericParseJSON (jsonOptions "toolFunction")

instance ToJSON ToolFunction where
  toJSON = genericToJSON (jsonOptions "toolFunction")

data Model = Mercury3
  deriving (Show, Generic)

instance ToJSON Model where
  toJSON Mercury3 = String "mercury-3"

instance FromJSON Model where
  parseJSON = withObject "model" $ \v -> do
    model <- v .: "Model"
    case (model :: Text) of
      "mercury-3" -> pure Mercury3
      _ -> fail "unknown model"
