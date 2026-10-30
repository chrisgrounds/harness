module Main (main) where

import Agent (loadLlm)
import Control.Monad (filterM)
import LoadEnv (loadEnv)
import System.Environment (lookupEnv)
import qualified Core
import Core (Model (..), StartRun (..), allModels, modelId, modelName, newRun, provider, Provider (..))
import qualified Data.Text as T
import Holey (Config (..), OnFailure (..), runHoley)
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
      (model, failure) <- maybe (die usage) pure (options args Mercury3 Revert)
      loadEnv
      available <- filterM configured allModels
      let socketPath = "/tmp/harness.sock"
      putStrLn "opening server, listening..."
      Socket.serve socketPath model available $ \chosen token publish (StartRun dir req) -> do
        llm' <- case (chosen, token) of
          (OpenAIModel _, Just source) -> OpenAI.openaiLlmWithToken source
          _ -> loadLlm chosen
        run <- holey chosen failure llm'
        newProgress publish (newRun req) >>= run dir
  where
    usage =
      "Usage: server [--model MODEL] [--keep-failed]\nserver login openai [--new | --account CLIENT_ID]\nserver accounts openai | use openai CLIENT_ID | logout openai | models openai\n"
        <> "MODEL is mercury (the default, with INCEPTION_API_KEY), or opus, sonnet or haiku (Claude, with ANTHROPIC_API_KEY).\n"
        <> "OpenAI models use --model openai:SLUG; login openai and models openai first.\n"
        <> "A run that does not finish undoes its changes; --keep-failed leaves them.\n"
        <> "The client says which project each run works on."
    options ("--model" : name : rest) _ failure = parseModel name >>= \m -> options rest m failure
    options ("--keep-failed" : rest) m _ = options rest m Keep
    options [] m failure = Just (m, failure)
    options _ _ _ = Nothing

parseModel :: String -> Maybe Model
parseModel name = case T.stripPrefix (T.pack "openai:") (T.pack name) of
  Just slug | not (T.null slug) -> Just (OpenAIModel slug)
  _ -> lookup name models

models :: [(String, Model)]
models =
  [(T.unpack (modelName m), m) | m <- allModels] ++ [(T.unpack (modelId m), m) | m <- allModels]

holey :: Model -> OnFailure -> (Core.Chat -> IO Core.Chat) -> IO (FilePath -> Progress -> IO ())
holey m failure llm' = do
  definitions <- loadToolDefinitions m
  putStrLn ("using " <> T.unpack (modelId m))
  pure $
    runHoley
      Config
        { llm = llm',
          llmModel = m,
          toolDefinitions = definitions,
          onFailure = failure
        }

configured :: Model -> IO Bool
configured model = case provider model of
  Inception -> present "INCEPTION_API_KEY"
  Anthropic -> present "ANTHROPIC_API_KEY"
  OpenAI -> pure False
  where
    present name = maybe False (not . null) <$> lookupEnv name
