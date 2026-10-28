{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Message (Message (..), textMessage) where

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
    tokensPerSecond :: Maybe Double,
    -- | A reply exactly as its provider sent it, such as Anthropic's content
    -- blocks with their thinking, to be sent back unchanged in later
    -- requests. Not part of the OpenAI-style JSON.
    providerContent :: Maybe Value
  }
  deriving (Eq, Show, Generic)

-- | A message of only text.
textMessage :: Role -> Text -> Message
textMessage r text = Message r (Just text) [] Nothing Nothing Nothing

instance FromJSON Message where
  parseJSON = withObject "Message" $ \value ->
    Message
      <$> value .: "role"
      <*> value .:? "content"
      <*> value .:? "tool_calls" .!= []
      <*> value .:? "tool_call_id"
      <*> value .:? "tokens_per_second"
      <*> pure Nothing

instance ToJSON Message where
  toJSON message =
    object $
      [ "role" .= role message,
        "content" .= content message,
        "tokens_per_second" .= tokensPerSecond message
      ]
        ++ ["tool_calls" .= toolCalls message | not (null (toolCalls message))]
        ++ ["tool_call_id" .= callId | Just callId <- [toolCallId message]]
