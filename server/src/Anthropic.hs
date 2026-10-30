{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Anthropic (anthropicLlm, requestBody, betas, replyMessage) where

import Control.Monad.IO.Class (liftIO)
import Control.Retry (exponentialBackoff, limitRetries)
import Core
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Req
import Network.HTTP.Types (statusCode)

anthropicLlm :: Text -> Chat -> IO Chat
anthropicLlm apiKey chat = do
  start <- getMonotonicTimeNSec
  response <- runReq config $
    req
      POST
      (https "api.anthropic.com" /: "v1" /: "messages")
      (ReqBodyJson (requestBody chat))
      jsonResponse
      ( header "x-api-key" (TE.encodeUtf8 apiKey)
          <> header "anthropic-version" "2026-10-21"
          <> header "anthropic-beta" (B.intercalate "," (betas (model chat)))
          <> responseTimeout (10 * 60 * 1000000)
      )
  end <- liftIO getMonotonicTimeNSec
  case parseEither replyMessage (responseBody response) of
    Left err -> ioError (userError ("Unexpected reply from the Anthropic API: " <> err))
    Right (message, outputTokens) ->
      let seconds = fromIntegral (end - start) / 1e9 :: Double
       in pure (addMessage message {tokensPerSecond = Just (fromIntegral outputTokens / seconds)} chat)
  where
    config =
      defaultHttpConfig
        { httpConfigRetryJudge = \_ r -> retryable (statusCode (responseStatus r)),
          httpConfigRetryPolicy = exponentialBackoff 2000000 <> limitRetries 4
        }
    retryable code = code `elem` [408, 409, 429] || code >= 500

betas :: Model -> [B.ByteString]
betas m = "thinking-binding-controls-2026-08-01" : ["server-side-fallback-2026-07-01" | hasFallbacks m]

hasFallbacks :: Model -> Bool
hasFallbacks m = m `elem` [ClaudeOpus, ClaudeSonnet]

requestBody :: Chat -> Value
requestBody chat =
  object $
    [ "model" .= modelId (model chat),
      "max_tokens" .= (16000 :: Int),
      "messages" .= conversation (reverse (messages chat)),
      "thinking" .= object ["type" .= ("adaptive" :: Text), "block_binding" .= object ["prefix_mismatch_behavior" .= ("drop_block" :: Text)]],
      "output_config" .= object ["effort" .= ("high" :: Text)],
      "cache_control" .= object ["type" .= ("ephemeral" :: Text)]
    ]
      ++ ["system" .= T.intercalate "\n\n" system | not (null system)]
      ++ ["tools" .= map tool (tools chat) | not (null (tools chat))]
      ++ ["fallbacks" .= ("default" :: Text) | hasFallbacks (model chat)]
  where
    system = [text | m <- messages (chatInOrder chat), role m == System, Just text <- [content m], not (T.null (T.strip text))]
    chatInOrder c = c {messages = reverse (messages c)}
    tool definition =
      let f = toolFunction definition
       in object
            [ "name" .= toolNameText (toolFunctionName f),
              "description" .= toolFunctionDescription f,
              "input_schema" .= toolFunctionParameters f
            ]

conversation :: [Message] -> [Value]
conversation ms = [object ["role" .= r, "content" .= blocks] | (r, blocks) <- merge (filter (not . null . snd) (mapMaybe turn ms))]
  where
    turn m = case role m of
      System -> Nothing
      User -> Just ("user" :: Text, textBlock (content m))
      Tool -> Just ("user", [object ["type" .= ("tool_result" :: Text), "tool_use_id" .= fromMaybe "" (toolCallId m), "content" .= result (content m)]])
      Assistant -> Just ("assistant", maybe (built m) toList (providerContent m >>= asArray))
    built m = textBlock (content m) ++ map toolUse (toolCalls m)
    toolUse r =
      object
        [ "type" .= ("tool_use" :: Text),
          "id" .= toolRequestId r,
          "name" .= toolNameText (toolRequestFunctionParamsName (toolRequestFunction r)),
          "input" .= fromMaybe (object []) (decodeStrict (TE.encodeUtf8 (toolRequestFunctionParamsArguments (toolRequestFunction r))) :: Maybe Value)
        ]
    textBlock (Just text) | not (T.null (T.strip text)) = [object ["type" .= ("text" :: Text), "text" .= text]]
    textBlock _ = []
    result (Just text) | not (T.null text) = text
    result _ = "(no output)"
    asArray (Array blocks) = Just blocks
    asArray _ = Nothing
    merge ((r, a) : (r', b) : rest) | r == r' = merge ((r, a ++ b) : rest)
    merge (t : rest) = t : merge rest
    merge [] = []

replyMessage :: Value -> Parser (Message, Int)
replyMessage = withObject "message" $ \o -> do
  blocks <- o .: "content"
  stop <- o .:? "stop_reason"
  outputTokens <- o .: "usage" >>= (.: "output_tokens")
  if stop == Just ("refusal" :: Text)
    then do
      details <- o .:? "stop_details"
      category <- maybe (pure Nothing) (withObject "stop_details" (.:? "category")) details
      let note = "The model declined this request" <> maybe "" (\c -> " (" <> c <> ")") category <> "."
      pure (textMessage Assistant note, outputTokens)
    else do
      texts <- concat <$> mapM blockText blocks
      calls <- concat <$> mapM blockToolUse blocks
      pure (Message Assistant (Just (T.intercalate "\n" texts)) calls Nothing Nothing (Just (toJSON (blocks :: [Value]))), outputTokens)
  where
    blockType = withObject "block" (.: "type")
    blockText block = do
      kind <- blockType block
      if kind == ("text" :: Text) then withObject "text" (fmap pure . (.: "text")) block else pure []
    blockToolUse block = do
      kind <- blockType block
      if kind /= ("tool_use" :: Text)
        then pure []
        else flip (withObject "tool_use") block $ \b -> do
          callId <- b .: "id"
          name <- b .: "name"
          input <- b .: "input"
          toolName <- either (\_ -> fail ("Unknown tool " <> T.unpack name)) pure (parseEither parseJSON (String name))
          pure [ToolRequest callId FunctionTool (ToolRequestFunctionParams toolName (TE.decodeUtf8 (L.toStrict (encode (input :: Value)))))]
