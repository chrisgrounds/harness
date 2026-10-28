module Main (main) where

import Agent (inceptionLlm, loadApiKey)
import Core (Model (..), StartRun (..), newRun)
import FakeRun (fakeRun)
import Holey (Config (..), runHoley)
import Progress (Progress, newProgress)
import qualified Socket
import System.Environment (getArgs)
import System.Exit (die)
import Tools (registerTool, toolToApiDefinition)

main :: IO ()
main = do
  args <- getArgs
  run <- case args of
    [] -> holey
    ["--demo"] -> pure (const fakeRun)
    _ -> die "Usage: server [--demo]\nThe client says which project each run works on. --demo plays a scripted run without calling the model."
  let socketPath = "/tmp/harness.sock"
  putStrLn "opening server, listening..."
  -- Every requirement starts a run.
  Socket.serve socketPath $ \publish (StartRun dir req) -> newProgress publish (newRun req) >>= run dir

holey :: IO (FilePath -> Progress -> IO ())
holey = do
  apiKey <- loadApiKey
  definitions <- mapM (\(name, _) -> (,) name <$> toolToApiDefinition name) registerTool
  pure $
    runHoley
      Config
        { llm = inceptionLlm apiKey,
          llmModel = Mercury3,
          toolDefinitions = definitions
        }
