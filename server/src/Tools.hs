{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tools where

import Control.Exception (IOException, try)
import Core
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Paths_server (getDataFileName)
import qualified System.Directory as TIO

runTool :: ToolName -> ToolRequestFunctionParams -> IO (Either IOException Text)
runTool toolName toolDetails = do
  case toolName of
    ReadFile -> do
      let args = toolRequestFunctionParamsArguments toolDetails
      ReadFilePath path <-
        either fail pure (eitherDecodeStrict (TE.encodeUtf8 args))
      try $ TIO.readFile (T.unpack path)
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

registerTool :: [(ToolName, FilePath)]
registerTool =
  [ (ReadFile, "./tools/read_file.json"),
    (Discover, "./tools/discover.json")
  ]

loadToolDefinition :: FilePath -> IO ToolDefinition
loadToolDefinition path = do
  fileContent <- getDataFileName path >>= TIO.readFile
  case eitherDecodeStrict (TE.encodeUtf8 fileContent) of
    Left err -> fail ("Failed to parse ToolDefinition JSON from " <> path <> ": " <> err)
    Right def -> pure def

toolToApiDefinition :: ToolName -> IO ToolDefinition
toolToApiDefinition toolName =
  case lookup toolName registerTool of
    Nothing -> fail ("No tool definition registered for " <> show toolName)
    Just path -> loadToolDefinition path
