{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent
  ( ApiKey,
    Llm,
    loadApiKey,
    inceptionLlm,
    runAgent,
    toolMessage,
  )
where

import Control.Monad.IO.Class (liftIO)
import Core
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import LoadEnv
import Network.HTTP.Req
import OpenAI
import System.Environment

newtype ApiKey = ApiKey String

-- | Sends a chat to a model and returns it with the model's reply added.
type Llm = Chat -> IO Chat

data ApiDefinition = ApiDefinition (Url 'Https)

toApiDefinition :: Model -> ApiDefinition
toApiDefinition Mercury3 = ApiDefinition (https "api.inceptionlabs.ai" /: "v1" /: "chat" /: "completions")

-- | Reads @INCEPTION_API_KEY@ from the environment or a @.env@ file.
loadApiKey :: IO ApiKey
loadApiKey = loadEnv >> ApiKey <$> getEnv "INCEPTION_API_KEY"

inceptionLlm :: ApiKey -> Llm
inceptionLlm = callLLM

-- | Runs the model, answering its tool requests with the handler, until it
-- replies without requesting tools or has used its turn budget.
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
toolMessage callId result =
  Message
    { role = Tool,
      content = Just result,
      toolCalls = [],
      toolCallId = Just callId,
      tokensPerSecond = Nothing
    }

callLLM :: ApiKey -> Chat -> IO Chat
callLLM (ApiKey apiKey) chat = do
  let ApiDefinition url = toApiDefinition (model chat)

  -- the `Chat` type here stores most recent messages at the beginning of [Message]
  -- whereas the LLM expects the reverse
  let llmChat = chat {messages = reverse $ messages chat}

  runReq defaultHttpConfig $ do
    start <- liftIO getMonotonicTimeNSec

    res <- req POST url (ReqBodyJson llmChat) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))
    let parsedRes = responseBody res :: OpenAIChatCompletionResponse

    end <- liftIO getMonotonicTimeNSec

    let tokensUsed = usageCompletionTokens . responseUsage $ parsedRes

    case listToMaybe (responseChoices parsedRes) of
      Nothing -> pure chat
      Just choice -> do
        let responseMessage = choiceMessage choice
        let msg =
              Message
                { role = messageRole responseMessage,
                  content = messageContent responseMessage,
                  toolCalls = fromMaybe [] (messageToolCalls responseMessage),
                  toolCallId = Nothing,
                  tokensPerSecond = Just (calculateTokensPerSecond tokensUsed (fromIntegral (end - start)))
                }

        pure $ addMessage msg chat

-- divide by 1b because interval is in nanoseconds
calculateTokensPerSecond :: Int -> Double -> Double
calculateTokensPerSecond tokensUsed interval = fromIntegral tokensUsed / (interval / 1e9)
