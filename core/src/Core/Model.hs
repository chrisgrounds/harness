{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Model (Model (..)) where

import Data.Aeson
import GHC.Generics (Generic)

data Model = Mercury3
  deriving (Eq, Show, Generic)

instance ToJSON Model where
  toJSON Mercury3 = String "mercury-3"

instance FromJSON Model where
  parseJSON = withText "Model" $ \m ->
    case m of
      "mercury-3" -> pure Mercury3
      _ -> fail "unknown model"
