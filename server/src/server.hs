module Main (main) where

import Agent (loadLlm)
import Core (Model (..), StartRun (..), allModels, modelId, newRun)
import qualified Data.Text as T
import Holey (Config (..), OnFailure (..), runHoley)
import Progress (Progress, newProgress)
import qualified Socket
import System.Environment (getArgs)
import System.Exit (die)
import Tools (loadToolDefinitions)

main :: IO ()
main = do
  args <- getArgs
  (model, failure) <- maybe (die usage) pure (options args Mercury3 Revert)
  run <- holey model failure
  let socketPath = "/tmp/harness.sock"
  putStrLn "opening server, listening..."
  -- Every requirement starts a run.
  Socket.serve socketPath model $ \publish (StartRun dir req) -> newProgress publish (newRun req) >>= run dir
  where
    usage =
      "Usage: server [--model MODEL] [--keep-failed]\n"
        <> "MODEL is mercury (the default, with INCEPTION_API_KEY), or opus, sonnet or haiku (Claude, with ANTHROPIC_API_KEY).\n"
        <> "A run that does not finish undoes its changes; --keep-failed leaves them.\n"
        <> "The client says which project each run works on."
    options ("--model" : name : rest) _ failure = lookup name models >>= \m -> options rest m failure
    options ("--keep-failed" : rest) m _ = options rest m Keep
    options [] m failure = Just (m, failure)
    options _ _ _ = Nothing

-- | Short names and full model names.
models :: [(String, Model)]
models =
  [("mercury", Mercury3), ("opus", ClaudeOpus), ("sonnet", ClaudeSonnet), ("haiku", ClaudeHaiku)]
    ++ [(T.unpack (modelId m), m) | m <- allModels]

holey :: Model -> OnFailure -> IO (FilePath -> Progress -> IO ())
holey m failure = do
  llm' <- loadLlm m
  definitions <- loadToolDefinitions
  putStrLn ("using " <> T.unpack (modelId m))
  pure $
    runHoley
      Config
        { llm = llm',
          llmModel = m,
          toolDefinitions = definitions,
          onFailure = failure
        }
