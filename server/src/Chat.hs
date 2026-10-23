{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Chat
  ( ChatPayload (..)
  , Message (..)
  , Model (..)
  , Tool (..)
  , ToolFunction (..)
  , addMessage
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import OpenAI (Role, ToolName, ToolRequest, ToolType)

data ChatPayload = ChatPayload
  { messages :: [Message],
    model :: Model,
    tools :: [Tool]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message
  { role :: Role,
    content :: Maybe Text,
    toolCalls :: [ToolRequest]
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \value ->
    Message
      <$> value .: "role"
      <*> value .:? "content"
      <*> value .:? "tool_calls" .!= []

instance ToJSON Message where
  toJSON message =
    object $
      [ "role" .= role message,
        "content" .= content message
      ]
        ++ ["tool_calls" .= toolCalls message | not (null (toolCalls message))]

addMessage :: Message -> ChatPayload -> ChatPayload
addMessage m c = c {messages = m : messages c}

data Tool = Tool
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

instance FromJSON Tool where
  parseJSON = genericParseJSON (jsonOptions "tool")

instance ToJSON Tool where
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
