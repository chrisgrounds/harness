module Main (main) where

import Agent (inceptionLlm, loadApiKey)
import Core (Model (..), newRun)
import FakeRun (fakeRun)
import Holey (Config (..), runHoley)
import Progress (Progress, newProgress)
import qualified Socket
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import Tools (registerTool, toolToApiDefinition)

main :: IO ()
main = do
  args <- getArgs
  run <- case args of
    [] -> holey
    ["--demo"] -> pure fakeRun
    _ -> die "Usage: server [--demo]\nRun from the root of the project to work on. --demo plays a scripted run without calling the model."
  let socketPath = "/tmp/harness.sock"
  putStrLn "opening server, listening..."
  -- Every requirement starts a run.
  Socket.serve socketPath $ \publish req -> newProgress publish (newRun req) >>= run

holey :: IO (Progress -> IO ())
holey = do
  apiKey <- loadApiKey
  definitions <- mapM (\(name, _) -> (,) name <$> toolToApiDefinition name) registerTool
  root <- getCurrentDirectory
  pure $
    runHoley
      Config
        { llm = inceptionLlm apiKey,
          llmModel = Mercury3,
          toolDefinitions = definitions,
          projectRoot = root
        }
