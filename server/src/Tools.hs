{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tools where

import Chat
import Control.Exception (IOException, try)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import OpenAI
import qualified System.Directory as TIO

runTool :: ToolName -> ToolRequestFunctionParams -> IO (Either IOException Text)
runTool toolName toolDetails = do
  case toolName of
    ReadFile -> do
      let args = toolRequestFunctionParamsArguments toolDetails
      ReadFilePath path <-
        either fail pure (eitherDecodeStrict (TE.encodeUtf8 args))
      -- For now we make this a relative path
      -- we should really handle this much better
      try $ TIO.readFile ("../" <> T.unpack path)
    Discover -> do
      let args = toolRequestFunctionParamsArguments toolDetails
      ReadFilePath path <-
        either fail pure (eitherDecodeStrict (TE.encodeUtf8 args))
      try $ do
        filenames <- TIO.listDirectory (T.unpack path)
        pure $ T.unlines $ T.pack <$> filenames

data ReadFilePath = ReadFilePath {readPath :: Text}
  deriving (Show)

instance FromJSON ReadFilePath where
  parseJSON = withObject "ReadFilePath" $ \value ->
    ReadFilePath <$> value .: "path"

toolToApiDefinition :: ToolName -> ToolDefinition
toolToApiDefinition toolName = case toolName of
  ReadFile ->
    ToolDefinition
      { toolType = FunctionTool,
        toolFunction =
          ToolFunction
            { toolFunctionName = ReadFile,
              toolFunctionDescription = "Reads a file",
              toolFunctionParameters = pathParams
            }
      }
  Discover ->
    ToolDefinition
      { toolType = FunctionTool,
        toolFunction =
          ToolFunction
            { toolFunctionName = Discover,
              toolFunctionDescription = "Lists files in a directory",
              toolFunctionParameters = pathParams
            }
      }
  where
    pathParams =
      object
        [ "type" .= ("string" :: Text),
          "properties"
            .= object
              [ "path"
                  .= object
                    [ "type" .= ("string" :: Text),
                      "description" .= ("Workspace relative path" :: Text)
                    ]
              ],
          "required" .= (["path"] :: [Text]),
          "additionalProperties" .= False
        ]
