{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Tool
  ( ToolCallId,
    ToolName (..),
    toolNameText,
    ToolType (..),
    ToolRequestFunctionParams (..),
    ToolRequest (..),
    ToolFunction (..),
    ToolDefinition (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

type ToolCallId = Text

data ToolName
  = ReadFile
  | Discover
  | Search
  | SetTypes
  | SetFunctions
  | SetProperties
  | ImplementFunction
  deriving (Eq, Ord, Show, Enum, Bounded)

instance FromJSON ToolName where
  parseJSON = withText "ToolName" $ \value ->
    case lookup value [(toolNameText name, name) | name <- [minBound .. maxBound]] of
      Just name -> pure name
      Nothing -> fail "unknown tool name"

instance ToJSON ToolName where
  toJSON = String . toolNameText

toolNameText :: ToolName -> Text
toolNameText = T.pack . show

data ToolType = FunctionTool
  deriving (Eq, Show)

instance FromJSON ToolType where
  parseJSON = withText "ToolType" $ \value ->
    case value of
      "function" -> pure FunctionTool
      _ -> fail "unknown type"

instance ToJSON ToolType where
  toJSON FunctionTool = String "function"

data ToolRequestFunctionParams = ToolRequestFunctionParams
  { toolRequestFunctionParamsName :: ToolName,
    toolRequestFunctionParamsArguments :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToolRequestFunctionParams where
  parseJSON = genericParseJSON (jsonOptions "toolRequestFunctionParams")

instance ToJSON ToolRequestFunctionParams where
  toJSON = genericToJSON (jsonOptions "toolRequestFunctionParams")

data ToolRequest = ToolRequest
  { toolRequestId :: ToolCallId,
    toolRequestType :: ToolType,
    toolRequestFunction :: ToolRequestFunctionParams
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToolRequest where
  parseJSON = genericParseJSON (jsonOptions "toolRequest")

instance ToJSON ToolRequest where
  toJSON = genericToJSON (jsonOptions "toolRequest")

data ToolFunction = ToolFunction
  { toolFunctionName :: ToolName,
    toolFunctionDescription :: Text,
    toolFunctionParameters :: Value
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToolFunction where
  parseJSON = genericParseJSON (jsonOptions "toolFunction")

instance ToJSON ToolFunction where
  toJSON = genericToJSON (jsonOptions "toolFunction")

data ToolDefinition = ToolDefinition
  { toolType :: ToolType,
    toolFunction :: ToolFunction
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToolDefinition where
  parseJSON = genericParseJSON (jsonOptions "tool")

instance ToJSON ToolDefinition where
  toJSON = genericToJSON (jsonOptions "tool")

jsonOptions :: String -> Options
jsonOptions prefix =
  defaultOptions
    { fieldLabelModifier =
        camelTo2 '_' . Prelude.drop (Prelude.length prefix)
    }
