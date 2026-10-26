{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Role (Role (..)) where

import Data.Aeson
import GHC.Generics (Generic)

data Role = User | Assistant | System | Tool
  deriving (Eq, Show, Generic)

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
