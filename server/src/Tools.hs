{-# LANGUAGE OverloadedStrings #-}

module Tools (loadToolDefinitions, readOnlyTool) where

import Control.Exception (IOException, try)
import Core
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Paths_server (getDataFileName)
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

offerImplementers :: [Model] -> ToolDefinition -> ToolDefinition
offerImplementers offered d = d {toolFunction = f {toolFunctionParameters = edit (toolFunctionParameters f)}}
  where
    f = toolFunction d
    edit = at "properties" . at "functions" . at "items" . at "properties" $ case offered of
      _ : _ : _ -> at "model" (at' (KeyMap.insert "enum" (toJSON (map modelName offered))))
      _ -> at' (KeyMap.delete "model")
    at key g = at' (KeyMap.mapWithKey (\k v -> if k == key then g v else v))
    at' g (Object o) = Object (g o)
    at' _ v = v

readOnlyTool :: FilePath -> ToolName -> Text -> Maybe (IO Text)
readOnlyTool root ReadFile args = Just $ withArgument "path" args $ \path ->
  reportIOError ("Could not read " <> path) (TIO.readFile (root </> T.unpack path))
readOnlyTool root ListFiles args = Just $ withArgument "path" args $ \path ->
  reportIOError ("Could not list " <> path) (T.unlines . map T.pack <$> Dir.listDirectory (root </> T.unpack path))
readOnlyTool root RipGrep args = Just $ withArgument "pattern" args (ripGrep root)
readOnlyTool _ _ _ = Nothing

ripGrep :: FilePath -> Text -> IO Text
ripGrep root pattern = do
  result <-
    try $
      readCreateProcessWithExitCode
        (proc "rg" ["--no-config", "--line-number", "--with-filename", "--no-heading", "--color=never", "--fixed-strings", "--sort=path", "--glob=*.hs", "--glob=*.cabal", "--glob=!dist-newstyle", "--", T.unpack pattern, "."]) {cwd = Just root}
        ""
  pure $ case result of
    Left err -> "RipGrep failed: " <> T.pack (show (err :: IOException))
    Right (ExitSuccess, out, _) -> truncateLines 200 (T.unlines (map relative (T.lines (T.pack out))))
    Right (ExitFailure 1, _, _) -> "No matches."
    Right (ExitFailure _, _, err) -> "RipGrep failed: " <> T.pack err
  where
    relative line = fromMaybe line (T.stripPrefix "./" line)

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

loadToolDefinitions :: Model -> IO (ToolName -> ToolDefinition)
loadToolDefinitions m = do
  loaded <- Map.adjust (offerImplementers (implementers m)) SetFunctions . Map.fromList <$> mapM (\name -> (,) name <$> load name) [minBound .. maxBound]
  pure (loaded Map.!)
  where
    load name = do
      let path = "tools" </> camelTo2 '_' (show name) <.> "json"
      contents <- getDataFileName path >>= TIO.readFile
      either (\err -> fail ("Failed to parse the tool definition " <> path <> ": " <> err)) pure (eitherDecodeStrict (TE.encodeUtf8 contents))
