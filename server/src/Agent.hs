{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent
  ( ApiKey (..),
    loadAgentConfig,
    handleMessage,
    callLLM,
  )
where

import Control.Monad.IO.Class (liftIO)
import Core
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import LoadEnv
import Network.HTTP.Req
import OpenAI
import System.Environment
import Tools

newtype BaseUrl = BaseUrl Text
  deriving (Show)

newtype Uri = Uri Text
  deriving (Show)

newtype ApiKey = ApiKey String
  deriving (Show)

data ApiDefinition = ApiDefinition (Url 'Https)
  deriving (Show)

toApiDefinition :: Model -> ApiDefinition
toApiDefinition Mercury3 = ApiDefinition (https "api.inceptionlabs.ai" /: "v1" /: "chat" /: "completions")

-- | Reads the API key and builds an empty chat with the registered tools.
loadAgentConfig :: IO (ApiKey, Chat)
loadAgentConfig = do
  loadEnv
  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"
  -- Don't hardcode this
  let llmModel = Mercury3
  toolDefinitions <- mapM toolToApiDefinition (map fst registerTool)
  pure (apiKey, Chat [] llmModel toolDefinitions)

handleMessage :: ApiKey -> Chat -> Message -> IO Chat
handleMessage apiKey chat message = do
  let newChat = addMessage message chat
  newerChat <- callLLM apiKey newChat
  talkWithToolRequests apiKey (getLatestToolRequests newerChat) newerChat

talkWithToolRequests :: ApiKey -> [ToolRequest] -> Chat -> IO Chat
talkWithToolRequests _ [] chat = pure chat
talkWithToolRequests apiKey toolRequests chat = do
  -- can we run these in parallel?
  toolCallResults <- mapM runToolRequest toolRequests

  if length toolCallResults == 0
    then pure chat
    else do
      let toolMessages =
            [ Message
                { role = Tool,
                  content = Just toolResult,
                  toolCalls = [],
                  toolCallId = Just tCallId,
                  tokensPerSecond = Nothing
                }
            | (tCallId, toolResult) <- toolCallResults
            ]

      let newChat = addMessages toolMessages chat

      chatWithAgentResponse <- callLLM apiKey newChat

      talkWithToolRequests apiKey (getLatestToolRequests chatWithAgentResponse) chatWithAgentResponse

runToolRequest :: ToolRequest -> IO (ToolCallId, Text)
runToolRequest tool = do
  let toolDetails = toolRequestFunction tool
  res <- runTool (toolRequestFunctionParamsName toolDetails) toolDetails

  case res of
    -- this error message is specific for ReadFile and does not apply to Discover, for example
    Left err -> pure (toolRequestId tool, "Could not read file: " <> T.pack (show err))
    Right contents -> pure (toolRequestId tool, contents)

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
