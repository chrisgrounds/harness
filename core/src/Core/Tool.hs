{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Tool
  ( ToolCallId,
    ToolName (..),
    ToolType (..),
    ToolRequestFunctionParams (..),
    ToolRequest (..),
    ToolFunction (..),
    ToolDefinition (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

type ToolCallId = Text

data ToolName = ReadFile | Discover
  deriving (Eq, Show)

instance FromJSON ToolName where
  parseJSON = withText "ToolName" $ \value ->
    case value of
      "ReadFile" -> pure ReadFile
      "Discover" -> pure Discover
      _ -> fail "unknown tool name"

instance ToJSON ToolName where
  toJSON ReadFile = String "ReadFile"
  toJSON Discover = String "Discover"

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
