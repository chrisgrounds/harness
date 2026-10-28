{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Role (Role (..)) where

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics (Generic)

-- | Encoded in lower case, as @"user"@.
data Role = User | Assistant | System | Tool
  deriving (Eq, Show, Generic)

instance ToJSON Role where
  toJSON = genericToJSON lowerCase

instance FromJSON Role where
  parseJSON = genericParseJSON lowerCase

lowerCase :: Options
lowerCase = defaultOptions {constructorTagModifier = map toLower}
