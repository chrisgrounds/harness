{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Fixed-origin HTTPS requests with credential-safe transport errors.
module OpenAI.Http (newManager, getJSON, postForm, request, withResponse, checkStatus, parseJSONBody, OAuthFailure (..)) where

import Control.Exception (Exception, catch, throwIO)
import Data.Typeable (Typeable)
import Data.Aeson (FromJSON, Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client hiding (newManager, withResponse)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)

data OAuthFailure = OAuthFailure Int T.Text
  deriving (Typeable)
instance Show OAuthFailure where
  show (OAuthFailure code reason) = "OpenAI authorization failed (HTTP " <> show code <> ", " <> T.unpack reason <> "). Run server login openai to reconnect."
instance Exception OAuthFailure

newManager :: IO Manager
newManager = HTTP.newManager tlsManagerSettings

request :: String -> IO Request
request url = do
  r <- parseRequest url
  pure r {redirectCount = 0, checkResponse = \_ _ -> pure (), responseTimeout = responseTimeoutMicro (300 * 1000000)}

withResponse :: Request -> Manager -> (Response BodyReader -> IO a) -> IO a
withResponse r manager action = HTTP.withResponse r manager action `catch` transportError
  where
    -- HttpException's Show includes the request and its bearer header.
    transportError :: HttpException -> IO a
    transportError _ = fail "OpenAI connection failed. Check your network and try again."

getJSON :: FromJSON a => Manager -> String -> Maybe T.Text -> IO a
getJSON manager url token = do
  r <- request url
  withResponse r {requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 t) | Just t <- [token]]} manager $ \response -> do
    body <- L.fromChunks <$> brConsume (responseBody response)
    checkStatus response body
    parseJSONBody body

postForm :: Manager -> String -> [(B.ByteString, B.ByteString)] -> IO L.ByteString
postForm manager url fields = do
  r <- urlEncodedBody fields <$> request url
  withResponse r {method = "POST"} manager $ \response -> do
    body <- L.fromChunks <$> brConsume (responseBody response)
    -- Token replies must never appear in errors or parser diagnostics.
    let code = statusCode (responseStatus response)
    if code >= 200 && code < 300 then pure body
      else throwIO (OAuthFailure code (T.pack (oauthCode body)))
  where
    oauthCode body = case eitherDecode body of
      Right (Object o) | Just (String code) <- KM.lookup "error" o -> T.unpack (T.filter (\c -> c >= 'a' && c <= 'z' || c == '_') (T.take 80 code))
      _ -> "request rejected"

checkStatus :: Response body -> L.ByteString -> IO ()
checkStatus response body = do
  let code = statusCode (responseStatus response)
      requestId = maybe "" ("; request ID " <>) (lookup "x-request-id" (responseHeaders response))
  if code >= 200 && code < 300 then pure ()
    else fail ("OpenAI HTTP " <> show code <> BC.unpack requestId <> ": " <> take 4000 (T.unpack (TE.decodeUtf8With (\_ _ -> Just '\xfffd') (L.toStrict body))))

parseJSONBody :: FromJSON a => L.ByteString -> IO a
parseJSONBody = either (const (fail "OpenAI returned an invalid JSON response.")) pure . eitherDecode
