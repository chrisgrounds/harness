module Main (main) where

import Agent (loadLlm)
import Control.Monad (filterM)
import LoadEnv (loadEnv)
import System.Environment (lookupEnv)
import qualified Core
import Core (Model (..), StartRun (..), allModels, modelId, modelName, newRun, provider, Provider (..))
import qualified Data.Text as T
import Holey (Config (..), OnFailure (..), runHoley, runLogFile)
import qualified OpenAI
import qualified OpenAI.Auth as Auth
import Progress (Progress, newProgress)
import qualified Socket
import System.Environment (getArgs)
import System.Exit (die)
import Tools (loadToolDefinitions)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["login", "openai"] -> Auth.login Nothing False
    ["login", "openai", "--new"] -> Auth.login Nothing True
    ["login", "openai", "--account", name] -> Auth.login (Just (T.pack name)) False
    ["logout", "openai"] -> Auth.logout
    ["accounts", "openai"] -> Auth.status
    ["use", "openai", name] -> Auth.useAccount (T.pack name)
    ["models", "openai"] -> OpenAI.listModels
    _ -> do
      (model, failure, socketPath) <- maybe (die usage) pure (options args Mercury3 Revert "/tmp/harness.sock")
      loadEnv
      available <- filterM configured allModels
      putStrLn "opening server, listening..."
      Socket.serve socketPath model available $ \chat chosen token publish (StartRun dir req effort) -> do
        llm' <- case (chosen, token) of
          (OpenAIModel _, Just source) -> OpenAI.openaiLlmWithToken effort source
          (OpenAIModel _, Nothing) -> OpenAI.openaiLlm effort
          _ -> loadLlm chosen
        log' <- runLogFile chat
        run <- holey chosen failure log' llm'
        newProgress publish (newRun req) >>= run dir
  where
    usage =
      "Usage: server [--model MODEL] [--keep-failed] [--socket PATH]\nserver login openai [--new | --account CLIENT_ID]\nserver accounts openai | use openai CLIENT_ID | logout openai | models openai\n"
        <> "MODEL is mercury (the default, with INCEPTION_API_KEY), or opus, sonnet or haiku (Claude, with ANTHROPIC_API_KEY).\n"
        <> "OpenAI models use --model openai:SLUG; login openai and models openai first.\n"
        <> "A run that does not finish undoes its changes; --keep-failed leaves them.\n"
        <> "The client says which project each run works on."
    options ("--model" : name : rest) _ failure path = parseModel name >>= \m -> options rest m failure path
    options ("--keep-failed" : rest) m _ path = options rest m Keep path
    options ("--socket" : path : rest) m failure _ = options rest m failure path
    options [] m failure path = Just (m, failure, path)
    options _ _ _ _ = Nothing

parseModel :: String -> Maybe Model
parseModel name = case T.stripPrefix (T.pack "openai:") (T.pack name) of
  Just slug | not (T.null slug) -> Just (OpenAIModel slug)
  _ -> lookup name models

models :: [(String, Model)]
models =
  [(T.unpack (modelName m), m) | m <- allModels] ++ [(T.unpack (modelId m), m) | m <- allModels]

holey :: Model -> OnFailure -> FilePath -> (Core.Chat -> IO Core.Chat) -> IO (FilePath -> Progress -> IO ())
holey m failure log' llm' = do
  definitions <- loadToolDefinitions m
  putStrLn ("using " <> T.unpack (modelId m))
  pure $
    runHoley
      Config
        { llm = llm',
          llmModel = m,
          toolDefinitions = definitions,
          onFailure = failure,
          logFile = log'
        }

configured :: Model -> IO Bool
configured model = case provider model of
  Inception -> present "INCEPTION_API_KEY"
  Anthropic -> present "ANTHROPIC_API_KEY"
  OpenAI -> pure False
  where
    present name = maybe False (not . null) <$> lookupEnv name
