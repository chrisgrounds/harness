{-# LANGUAGE OverloadedStrings #-}

module Tools (registerTool, toolToApiDefinition, readOnlyTool) where

import Control.Exception (IOException, try)
import Core
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Paths_server (getDataFileName)
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Runs a tool that only reads the workspace, or 'Nothing' for tools that
-- change the run and are handled by the orchestrator.
readOnlyTool :: ToolName -> Text -> Maybe (IO Text)
readOnlyTool ReadFile args = Just $ withArgument "path" args $ \path ->
  reportIOError ("Could not read " <> path) (TIO.readFile (T.unpack path))
readOnlyTool Discover args = Just $ withArgument "path" args $ \path ->
  reportIOError ("Could not list " <> path) (T.unlines . map T.pack <$> Dir.listDirectory (T.unpack path))
readOnlyTool Search args = Just $ withArgument "pattern" args search
readOnlyTool _ _ = Nothing

search :: Text -> IO Text
search pattern = do
  (code, out, err) <-
    readProcessWithExitCode
      "grep"
      ["-rnIF", "--include=*.hs", "--include=*.cabal", "--exclude-dir=dist-newstyle", "--exclude-dir=.git", "--", T.unpack pattern, "."]
      ""
  pure $ case code of
    ExitSuccess -> truncateLines 200 (T.pack out)
    ExitFailure 1 -> "No matches."
    ExitFailure _ -> "Search failed: " <> T.pack err

truncateLines :: Int -> Text -> Text
truncateLines n value
  | length ls > n = T.unlines (take n ls) <> "... " <> T.pack (show (length ls - n)) <> " more lines"
  | otherwise = value
  where
    ls = T.lines value

withArgument :: Text -> Text -> (Text -> IO Text) -> IO Text
withArgument key args run =
  case eitherDecodeStrict (TE.encodeUtf8 args) >>= parseEither (withObject "arguments" (.: Key.fromText key)) of
    Left err -> pure ("Invalid arguments: " <> T.pack err)
    Right value -> run value

reportIOError :: Text -> IO Text -> IO Text
reportIOError context action = do
  result <- try action
  pure $ case result of
    Left err -> context <> ": " <> T.pack (show (err :: IOException))
    Right value -> value

registerTool :: [(ToolName, FilePath)]
registerTool =
  [ (ReadFile, "./tools/read_file.json"),
    (Discover, "./tools/discover.json"),
    (Search, "./tools/search.json"),
    (SetTypes, "./tools/set_types.json"),
    (SetFunctions, "./tools/set_functions.json"),
    (SetProperties, "./tools/set_properties.json"),
    (ImplementFunction, "./tools/implement_function.json")
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
