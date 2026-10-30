{-# LANGUAGE OverloadedStrings #-}

module Agent
  ( Llm,
    loadLlm,
    runAgent,
    toolMessage,
  )
where

import Anthropic (anthropicLlm)
import Core
import Data.Text (Text)
import qualified Data.Text as T
import LoadEnv
import Inception (inceptionLlm)
import OpenAI (openaiLlm)
import System.Environment (lookupEnv)
import System.Exit (die)

type Llm = Chat -> IO Chat

loadLlm :: Model -> IO Llm
loadLlm m = do
  loadEnv
  case m of
    OpenAIModel _ -> openaiLlm Nothing
    Mercury3 -> inceptionLlm . T.pack <$> key "INCEPTION_API_KEY"
    _ -> anthropicLlm . T.pack <$> key "ANTHROPIC_API_KEY"
  where
    key name = lookupEnv name >>= maybe (die ("Set " <> name <> " to use " <> T.unpack (modelId m) <> ".")) pure

runAgent :: Llm -> Int -> (ToolRequest -> IO Text) -> Chat -> IO Chat
runAgent llm budget handle chat
  | budget <= 0 = pure chat
  | otherwise = do
      replied <- llm chat
      case getLatestToolRequests replied of
        [] -> pure replied
        requests -> do
          results <- mapM (\r -> toolMessage (toolRequestId r) <$> handle r) requests
          runAgent llm (budget - 1) handle (addMessages (reverse results) replied)

toolMessage :: ToolCallId -> Text -> Message
toolMessage callId result = (textMessage Tool result) {toolCallId = Just callId}
