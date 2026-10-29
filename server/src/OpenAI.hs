{-# LANGUAGE OverloadedStrings #-}

-- | Subscription-backed Responses API, using the harness's own tool loop.
module OpenAI (openaiLlm, openaiLlmWithToken, requestBody, responseReply, StreamState (..), emptyStream, feedStream, finishStream, listModels) where

import Core
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types (statusCode)
import Network.HTTP.Client hiding (requestBody, withResponse, newManager)
import qualified Network.HTTP.Client as HTTP
import qualified OpenAI.Auth as Auth
import qualified OpenAI.Http as Http

openaiLlm :: IO (Chat -> IO Chat)
openaiLlm = do
  manager <- Http.newManager
  -- Check login before the server starts accepting work.
  getToken <- Auth.tokenSource manager
  _ <- getToken
  openaiLlmWithToken getToken

-- | The GUI owns OAuth; only a short-lived bearer crosses the local socket.
openaiLlmWithToken :: IO Text -> IO (Chat -> IO Chat)
openaiLlmWithToken getToken = do
  manager <- Http.newManager
  pure $ \chat -> do
    token <- getToken
    r <- Http.request "https://api.openai.com/v1/responses"
    start <- getMonotonicTimeNSec
    response <- Http.withResponse r {method = "POST", requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token), ("Content-Type", "application/json"), ("Accept", "text/event-stream")], HTTP.requestBody = RequestBodyLBS (encode (requestBody chat))} manager $ \res -> do
      let readStream state = do
            chunk <- brRead (responseBody res)
            if B.null chunk then either fail pure (finishStream state)
              else either fail readStream (feedStream state chunk)
      if statusCode (responseStatus res) /= 200
        then do
          body <- L.fromChunks <$> brConsume (responseBody res)
          Http.checkStatus res body
          fail "OpenAI returned no event stream."
        else readStream emptyStream
    end <- getMonotonicTimeNSec
    (reply, tokens) <- either fail pure (parseEither responseReply response)
    let seconds = fromIntegral (end - start) / 1e9 :: Double
    pure (addMessage reply {tokensPerSecond = if seconds > 0 then Just (fromIntegral tokens / seconds) else Nothing} chat)

-- | System prompts become developer messages; output items, including encrypted
-- reasoning and namespaced calls, are replayed exactly as returned.
requestBody :: Chat -> Value
requestBody chat = object
  [ "model" .= modelId (model chat), "store" .= False, "stream" .= True,
    "include" .= ["reasoning.encrypted_content" :: Text],
    "input" .= concatMap input (reverse (messages chat)),
    "tools" .= [object ["type" .= ("namespace" :: Text), "name" .= ("harness" :: Text), "description" .= ("Tools for the current type-driven build step." :: Text), "tools" .= map definition (tools chat)] | not (null (tools chat))]
  ]
  where
    definition d = let f = toolFunction d in object
      ["type" .= ("function" :: Text), "name" .= toolNameText (toolFunctionName f), "description" .= toolFunctionDescription f, "parameters" .= toolFunctionParameters f, "strict" .= False]
    input m = case role m of
      Tool -> [object ["type" .= ("function_call_output" :: Text), "call_id" .= callId, "output" .= content m] | Just callId <- [toolCallId m]]
      Assistant | Just (Array items) <- providerContent m -> toList items
      Assistant -> [message "assistant" t | Just t <- [content m]] ++ map call (toolCalls m)
      System -> [message "developer" t | Just t <- [content m]]
      User -> [message "user" t | Just t <- [content m]]
    message r t = object ["role" .= (r :: Text), "content" .= t]
    call r = let f = toolRequestFunction r in object
      ["type" .= ("function_call" :: Text), "namespace" .= ("harness" :: Text), "name" .= toolNameText (toolRequestFunctionParamsName f), "call_id" .= toolRequestId r, "arguments" .= toolRequestFunctionParamsArguments f]

responseReply :: Value -> Parser (Message, Int)
responseReply = withObject "response" $ \o -> do
  status <- o .: "status" :: Parser Text
  if status /= "completed" then fail "OpenAI response did not complete." else pure ()
  items <- o .: "output" :: Parser [Value]
  calls <- catMaybes <$> mapM readCall items
  texts <- concat <$> mapM readText items
  tokens <- o .: "usage" >>= (.: "output_tokens")
  let text = T.intercalate "\n" texts
  pure ((textMessage Assistant text) {content = if T.null text then Nothing else Just text, toolCalls = calls, providerContent = Just (toJSON items)}, tokens)
  where
    readCall = withObject "output item" $ \o -> do
      kind <- o .: "type" :: Parser Text
      if kind /= "function_call" then pure Nothing else do
        namespace <- o .:? "namespace" :: Parser (Maybe Text)
        if namespace /= Just "harness" then fail "OpenAI called an unknown tool namespace." else pure ()
        name <- o .: "name"
        args <- o .: "arguments"
        callId <- o .: "call_id"
        pure (Just (ToolRequest callId FunctionTool (ToolRequestFunctionParams name args)))
    readText = withObject "output item" $ \o -> do
      kind <- o .: "type" :: Parser Text
      if kind /= "message" then pure [] else do
        blocks <- o .: "content"
        catMaybes <$> mapM (withObject "content" $ \b -> do
          t <- b .: "type" :: Parser Text
          case t of
            "output_text" -> Just <$> b .: "text"
            "refusal" -> Just <$> b .: "refusal"
            _ -> pure Nothing) blocks

-- | Incremental SSE framing. Arbitrary chunk and CRLF boundaries are accepted;
-- partial output never counts as a successful model reply.
data StreamState = StreamState B.ByteString [B.ByteString] (Maybe Value)
  deriving (Eq, Show)

emptyStream :: StreamState
emptyStream = StreamState B.empty [] Nothing

feedStream :: StreamState -> B.ByteString -> Either String StreamState
feedStream (StreamState pending fields completed) chunk = linesOf (pending <> chunk) fields completed
  where
    linesOf bytes ls done = case B.elemIndex 10 bytes of
      Nothing -> Right (StreamState bytes ls done)
      Just i -> do
        let line = B.takeWhile (/= 13) (B.take i bytes)
        (ls', done') <- if B.null line then (,) [] <$> event ls done
          else Right (if "data:" `B.isPrefixOf` line then ls ++ [B.dropWhile (== 32) (B.drop 5 line)] else ls, done)
        linesOf (B.drop (i + 1) bytes) ls' done'
    event [] done = Right done
    event ls done
      | B.intercalate "\n" ls == "[DONE]" = Right done
      | otherwise = do
          value <- either (const (Left "OpenAI sent invalid stream JSON.")) Right (eitherDecodeStrict (B.intercalate "\n" ls))
          withEvent value done
    withEvent (Object o) done = case KM.lookup "type" o of
      Just (String "response.completed") -> case KM.lookup "response" o of
        Just r -> Right (Just r)
        _ -> Left "OpenAI completion event had no response."
      Just (String kind) | kind `elem` ["response.failed", "response.incomplete", "error"] ->
        Left ("OpenAI stream failed: " <> T.unpack (TE.decodeUtf8 (L.toStrict (encode (Object o)))) <> " Check https://chatgpt.com/settings/usage for subscription limits.")
      _ -> Right done
    withEvent _ _ = Left "OpenAI sent an invalid stream event."

finishStream :: StreamState -> Either String Value
finishStream state@(StreamState pending fields _)
  | not (B.null pending) || not (null fields) = do
      StreamState _ _ result <- feedStream state "\n\n"
      maybe missing Right result
finishStream (StreamState _ _ result) = maybe missing Right result

missing :: Either String a
missing = Left "OpenAI stream ended without response.completed; the reply was not accepted."

listModels :: IO ()
listModels = do
  manager <- Http.newManager
  token <- Auth.accessToken manager
  body <- Http.getJSON manager "https://api.openai.com/v1/models" (Just token)
  choices <- either fail pure (parseEither catalog body)
  mapM_ (\(slug, name) -> putStrLn ("openai:" <> T.unpack slug <> "\t" <> T.unpack name)) choices
  where
    catalog = withObject "model catalog" $ \o -> do
      entries <- o .: "models"
      catMaybes <$> mapM (withObject "model" $ \m -> do
        visibility <- m .: "visibility" :: Parser Text
        if visibility == "list" then Just <$> ((,) <$> m .: "slug" <*> m .: "display_name") else pure Nothing) entries
