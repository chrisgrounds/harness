{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Chat (Chat (..), addMessage, addMessages, getLatestToolRequests) where

import Core.Message (Message (..))
import Core.Model (Model)
import Core.Tool (ToolDefinition, ToolRequest)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)

data Chat = Chat
  { messages :: [Message],
    model :: Model,
    tools :: [ToolDefinition]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

addMessage :: Message -> Chat -> Chat
addMessage m c = c {messages = m : messages c}

addMessages :: [Message] -> Chat -> Chat
addMessages ms c = c {messages = ms ++ messages c}

getLatestToolRequests :: Chat -> [ToolRequest]
getLatestToolRequests = maybe [] toolCalls . listToMaybe . messages
