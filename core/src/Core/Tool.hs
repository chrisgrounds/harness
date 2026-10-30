{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Tool
  ( ToolCallId,
    ToolName (..),
    toolNameText,
    ToolType (..),
    ToolRequestFunctionParams (..),
    ToolRequest (..),
    ToolCall (..),
    toolCallOf,
    ToolFunction (..),
    ToolDefinition (..),
  )
where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

type ToolCallId = Text

data ToolName
  = ReadFile
  | ListFiles
  | RipGrep
  | SetPlan
  | SetTypes
  | SetFunctions
  | SetProperties
  | ImplementFunction
  | Refactor
  | RequirementNotMet
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

-- | A tool request as the client shows it: the tool, and the path or
-- pattern it was called with, if it takes one.
data ToolCall = ToolCall
  { callTool :: ToolName,
    callSubject :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToolCall

instance ToJSON ToolCall

toolCallOf :: ToolRequest -> ToolCall
toolCallOf r = ToolCall (toolRequestFunctionParamsName f) (subject =<< decodeStrict (TE.encodeUtf8 (toolRequestFunctionParamsArguments f)))
  where
    f = toolRequestFunction r
    subject args = listToMaybe [s | key <- ["path", "pattern"], Just (String s) <- [KeyMap.lookup key args]]

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
