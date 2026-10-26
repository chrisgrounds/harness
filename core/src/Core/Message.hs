{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Message (Message (..)) where

import Core.Role (Role)
import Core.Tool (ToolCallId, ToolRequest)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Message = Message
  { role :: Role,
    content :: Maybe Text,
    toolCalls :: [ToolRequest],
    toolCallId :: Maybe ToolCallId,
    tokensPerSecond :: Maybe Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \value ->
    Message
      <$> value .: "role"
      <*> value .:? "content"
      <*> value .:? "tool_calls" .!= []
      <*> value .:? "tool_call_id"
      <*> value .:? "tokens_per_second"

instance ToJSON Message where
  toJSON message =
    object $
      [ "role" .= role message,
        "content" .= content message,
        "tokens_per_second" .= tokensPerSecond message
      ]
        ++ ["tool_calls" .= toolCalls message | not (null (toolCalls message))]
        ++ ["tool_call_id" .= callId | Just callId <- [toolCallId message]]
