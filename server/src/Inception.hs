{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Inception (inceptionLlm, completionReply) where

import Core
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Req

inceptionLlm :: Text -> Chat -> IO Chat
inceptionLlm apiKey chat = do
  start <- getMonotonicTimeNSec
  response <-
    runReq defaultHttpConfig $
      req
        POST
        (https "api.inceptionlabs.ai" /: "v1" /: "chat" /: "completions")
        (ReqBodyJson chat {messages = reverse (messages chat)})
        jsonResponse
        (header "Authorization" ("Bearer " <> TE.encodeUtf8 apiKey))
  end <- getMonotonicTimeNSec
  case parseEither completionReply (responseBody response) of
    Left err -> ioError (userError ("Unexpected reply from the Inception API: " <> err))
    Right Nothing -> pure chat
    Right (Just (message, tokens)) ->
      let seconds = fromIntegral (end - start) / 1e9 :: Double
       in pure (addMessage message {tokensPerSecond = Just (fromIntegral tokens / seconds)} chat)

completionReply :: Value -> Parser (Maybe (Message, Int))
completionReply = withObject "completion" $ \o -> do
  tokens <- o .: "usage" >>= (.: "completion_tokens")
  choices <- o .: "choices" :: Parser [Object]
  case choices of
    [] -> pure Nothing
    choice : _ -> (\message -> Just (message, tokens)) <$> (choice .: "message" :: Parser Message)
