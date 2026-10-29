{-# LANGUAGE OverloadedStrings #-}

module OpenAITests (openaiTests) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Core
import qualified Crypto.JWT as JWT
import Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Either (isLeft)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (ManagerSettings (..), Request (..))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import qualified OpenAI
import qualified OpenAI.Auth as Auth
import qualified OpenAI.Http as Http
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import System.Posix.Files (fileMode, getFileStatus)
import Data.Bits ((.&.))

openaiTests :: IO [String]
openaiTests = concat <$> sequence
  [ check "OpenAI request and reply preserve reasoning and tool history" historyTest,
    check "OpenAI streams tolerate every byte boundary and reject failures" streamTest,
    check "ChatGPT callback validates state and client binding" callbackTest,
    check "ChatGPT identity requires signature, issuer, audience, expiry and nonce" identityTest,
    check "ChatGPT refresh rotates once across concurrent workers and protects credentials" refreshTest,
    check "signed-out ChatGPT accounts are hidden while retaining reconnect registration" signedOutAccountTest
  ]
  where
    check name action = do
      putStrLn (name <> ":")
      result <- try (timeout (30 * 1000000) action >>= maybe (fail "Test timed out") pure) :: IO (Either IOException ())
      pure [name <> ": " <> show err | Left err <- [result]]

assert :: String -> Bool -> IO ()
assert label passed = unless passed (fail label)

field :: Text -> Value -> Maybe Value
field name (Object o) = KM.lookup (fromStringKey name) o
  where fromStringKey = Data.Aeson.Key.fromText
field _ _ = Nothing

outputItems :: [Value]
outputItems =
  [ object ["type" .= ("reasoning" :: Text), "id" .= ("rs_1" :: Text), "encrypted_content" .= ("opaque-reasoning" :: Text), "summary" .= ([] :: [Value])],
    object ["type" .= ("function_call" :: Text), "namespace" .= ("harness" :: Text), "name" .= ("ReadFile" :: Text), "arguments" .= ("{\"path\":\"A.hs\"}" :: Text), "call_id" .= ("call_1" :: Text), "id" .= ("fc_1" :: Text)]
  ]

completed :: Value
completed = object ["status" .= ("completed" :: Text), "output" .= outputItems, "usage" .= object ["output_tokens" .= (10 :: Int)]]

historyTest :: IO ()
historyTest = do
  (reply, count) <- either fail pure (parseEither OpenAI.responseReply completed)
  let m = OpenAIModel "future-account-slug"
      chat = Chat [reply, textMessage User "Read A.hs", textMessage System "Follow the build steps."] m []
      toolReply = (textMessage Tool "module A where") {toolCallId = Just "call_1"}
      body = OpenAI.requestBody (addMessage toolReply chat)
      expected = [object ["role" .= ("developer" :: Text), "content" .= ("Follow the build steps." :: Text)], object ["role" .= ("user" :: Text), "content" .= ("Read A.hs" :: Text)]] ++ outputItems ++ [object ["type" .= ("function_call_output" :: Text), "call_id" .= ("call_1" :: Text), "output" .= ("module A where" :: Text)]]
  assert "output retained" (providerContent reply == Just (toJSON outputItems) && count == 10)
  assert "correct tool call" (map toolRequestId (toolCalls reply) == ["call_1"])
  assert "chronological history and verbatim reasoning" (field "input" body == Just (toJSON expected))
  assert "subscription flags" (field "store" body == Just (Bool False) && field "stream" body == Just (Bool True))
  assert "model slug" (field "model" body == Just (String "future-account-slug"))
  assert "model protocol round-trip" (eitherDecode (encode m) == Right m)
  assert "no arbitrary capability ranking" (implementers m == [m] && defaultImplementer m == m && largerImplementer m m == m)
  let tool = ToolDefinition FunctionTool (ToolFunction ReadFile "Read a file" (object ["type" .= ("object" :: Text)]))
      withTools = OpenAI.requestBody chat {tools = [tool]}
  assert "namespace tool schema" (case field "tools" withTools of Just (Array a) -> any ((== Just (String "namespace")) . field "type") a; _ -> False)
  let unknown = object ["status" .= ("completed" :: Text), "output" .= [object ["type" .= ("function_call" :: Text), "namespace" .= ("other" :: Text), "name" .= ("ReadFile" :: Text)]], "usage" .= object ["output_tokens" .= (1 :: Int)]]
  assert "unknown namespace rejected" (isLeft (parseEither OpenAI.responseReply unknown))

streamTest :: IO ()
streamTest = do
  let event = L.toStrict (encode (object ["type" .= ("response.completed" :: Text), "response" .= completed]))
      wire = ": heartbeat\r\n\r\ndata: " <> event <> "\r\n\r\ndata: [DONE]\n\n"
      parseChunks = foldl (\state chunk -> state >>= (`OpenAI.feedStream` chunk)) (Right OpenAI.emptyStream)
  mapM_ (\i -> assert ("SSE split " <> show i) ((parseChunks [B.take i wire, B.drop i wire] >>= OpenAI.finishStream) == Right completed)) [0..B.length wire]
  assert "single byte chunks" ((parseChunks (map B.singleton (B.unpack wire)) >>= OpenAI.finishStream) == Right completed)
  assert "EOF without completion rejected" (isLeft (OpenAI.feedStream OpenAI.emptyStream "data: {\"type\":\"response.output_text.delta\",\"delta\":\"partial\"}\n\n" >>= OpenAI.finishStream))
  mapM_ (\kind -> assert "terminal failure rejected" (isLeft (OpenAI.feedStream OpenAI.emptyStream ("data: " <> L.toStrict (encode (object ["type" .= kind])) <> "\n\n")))) (["response.failed", "response.incomplete", "error"] :: [Text])
  assert "malformed JSON rejected" (isLeft (OpenAI.feedStream OpenAI.emptyStream "data: {\n\n"))

callbackTest :: IO ()
callbackTest = do
  let query = parseQuery "?state=expected&code=code&client_id=oaiapp_test"
  assert "new registration" (Auth.callbackResult "expected" Nothing query == Right ("code", "oaiapp_test"))
  assert "state mismatch" (isLeft (Auth.callbackResult "other" Nothing query))
  assert "client mismatch" (isLeft (Auth.callbackResult "expected" (Just "different") query))
  assert "duplicate state" (isLeft (Auth.callbackResult "expected" Nothing (("state", Just "expected"):query)))
  assert "denial" (isLeft (Auth.callbackResult "expected" Nothing (parseQuery "?state=expected&error=access_denied")))
  assert "missing client" (isLeft (Auth.callbackResult "expected" Nothing (parseQuery "?state=expected&code=x")))
  assert "returning client may be omitted" (Auth.callbackResult "expected" (Just "saved") (parseQuery "?state=expected&code=x") == Right ("x", "saved"))
  assert "RFC 7636 PKCE vector" (Auth.challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" == "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
  let newURL = Auth.authorizationURL "urn:uuid:host" Nothing "http://127.0.0.1:1234/auth/callback" "state" "nonce" "verifier"
      returning = Auth.authorizationURL "urn:uuid:host" (Just "saved") "http://127.0.0.1:1234/auth/callback" "state" "nonce" "verifier"
  assert "register only on first login" ("dynamic_agent_client" `T.isInfixOf` T.pack newURL && not ("agent_name_hint" `T.isInfixOf` T.pack returning))

identityTest :: IO ()
identityTest = do
  key <- JWT.genJWK (JWT.RSAGenParam 256)
  other <- JWT.genJWK (JWT.RSAGenParam 256)
  now <- getCurrentTime
  let seconds = realToFrac (utcTimeToPOSIXSeconds now) :: Double
      claims = object ["iss" .= ("https://auth.openai.com" :: Text), "aud" .= ("client" :: Text), "sub" .= ("subject" :: Text), "exp" .= (seconds + 3600), "iat" .= (seconds - 1), "nonce" .= ("nonce" :: Text)]
      sign value = do
        parsed <- either fail pure (parseEither parseJSON value :: Either String JWT.ClaimsSet)
        signed <- JWT.runJOSE (JWT.signClaims key (JWT.newJWSHeader (JWT.RequiredProtection, JWT.RS256)) parsed) :: IO (Either JWT.JWTError JWT.SignedJWT)
        either (const (fail "Fixture signing failed")) (pure . TE.decodeUtf8 . L.toStrict . JWT.encodeCompact) signed
      rejected keys token = do
        outcome <- try (Auth.verifyIdentityWithKeys keys "client" (Just "nonce") token) :: IO (Either IOException (Text, Maybe Text))
        assert "invalid token accepted" (isLeft outcome)
      alter name value (Object o) = Object (KM.insert name value o)
      alter _ _ value = value
  token <- sign claims
  valid <- Auth.verifyIdentityWithKeys (JWT.JWKSet [key]) "client" (Just "nonce") token
  assert "valid identity" (valid == ("subject", Nothing))
  rejected (JWT.JWKSet [other]) token
  mapM_ (\value -> sign value >>= rejected (JWT.JWKSet [key]))
    [alter "iss" (String "https://attacker.invalid") claims, alter "aud" (String "other") claims, alter "nonce" (String "other") claims, alter "exp" (toJSON (seconds - 10)) claims,
     case claims of Object o -> Object (KM.delete "exp" o); _ -> claims]

refreshTest :: IO ()
refreshTest = isolatedConfig $ do
  now <- getCurrentTime
  let old = Auth.Session "old-access" "old-refresh" "retained-id" (addUTCTime (-1) now) ["resource.invoke", "chatgpt.tokens.use.direct"]
      account = Auth.Account "saved-client" "subject" Nothing (Just old)
  Auth.withStore $ \file store -> Auth.saveStore file store {Auth.storeAccounts = Map.singleton "saved-client" account, Auth.storeActive = Just "saved-client"}
  count <- newIORef (0 :: Int)
  seen <- newIORef []
  responseCode <- newIORef status200
  let app req respond = do
        body <- strictRequestBody req
        modifyIORef' count (+1)
        writeIORef seen (parseQuery (L.toStrict body))
        code <- readIORef responseCode
        respond (responseLBS code [("Content-Type", "application/json")] (if code == status200 then encode (object ["token_type" .= ("Bearer" :: Text), "access_token" .= ("new-access" :: Text), "refresh_token" .= ("new-refresh" :: Text), "expires_in" .= (3600 :: Int)]) else "{\"error\":\"invalid_grant\"}"))
  testWithApplication (pure app) $ \localPort -> do
    manager <- HTTP.newManager HTTP.defaultManagerSettings {managerModifyRequest = \r -> pure r {host = "127.0.0.1", HTTP.port = localPort, secure = False}}
    tokens <- mapConcurrently (const (Auth.accessToken manager)) [1..4 :: Int]
    calls <- readIORef count
    form <- readIORef seen
    assert "one serialized refresh" (calls == 1 && tokens == replicate 4 "new-access")
    assert "refresh bound to client and resource" (lookup "client_id" form == Just (Just "saved-client") && lookup "resource" form == Just (Just "https://api.openai.com/v1") && lookup "refresh_token" form == Just (Just "old-refresh"))
    Auth.withStore $ \file store -> do
      let stored = Map.lookup "saved-client" (Auth.storeAccounts store) >>= Auth.accountSession
      assert "rotation stored atomically" (maybe False ((== "new-refresh") . Auth.sessionRefresh) stored)
      permissions <- fileMode <$> getFileStatus file
      assert "owner-only permissions" (permissions .&. 0o777 == 0o600)
      Auth.saveStore file store {Auth.storeAccounts = Map.singleton "saved-client" account}
    writeIORef responseCode status503
    temporary <- try (Auth.accessToken manager) :: IO (Either Http.OAuthFailure Text)
    assert "temporary failure propagated" (isLeft temporary)
    Auth.withStore $ \_ store -> assert "temporary failure retained credentials" (maybe False (not . isNothing . Auth.accountSession) (Map.lookup "saved-client" (Auth.storeAccounts store)))
    writeIORef responseCode status400
    terminal <- try (Auth.accessToken manager) :: IO (Either Http.OAuthFailure Text)
    assert "terminal failure propagated" (isLeft terminal)
    Auth.withStore $ \_ store -> assert "terminal failure cleared tokens" (maybe False (isNothing . Auth.accountSession) (Map.lookup "saved-client" (Auth.storeAccounts store)))

signedOutAccountTest :: IO ()
signedOutAccountTest = isolatedConfig $ do
  now <- getCurrentTime
  let session = Auth.Session "access" "refresh" "id" (addUTCTime 3600 now) ["resource.invoke", "chatgpt.tokens.use.direct"]
      account = Auth.Account "saved-client" "subject" (Just "person@example.com") (Just session)
      save value = Auth.withStore $ \file store -> Auth.saveStore file store
        {Auth.storeAccounts = Map.singleton "saved-client" value, Auth.storeActive = Just "saved-client"}
  save account
  visible <- Auth.accountViews
  assert "signed-in account is visible" (visible == [("saved-client", "person@example.com", True, True)])
  save account {Auth.accountSession = Nothing}
  signedOut <- Auth.accountViews
  assert "previously signed-out account is hidden" (null signedOut)
  Auth.logout
  afterLogout <- Auth.accountViews
  assert "sign-out keeps the account hidden" (null afterLogout)
  Auth.withStore $ \_ store -> do
    let retained = Map.lookup "saved-client" (Auth.storeAccounts store)
    assert "registration remains available for reconnect" (maybe False ((== "saved-client") . Auth.accountClient) retained)
    assert "retained registration has no credentials" (maybe False (isNothing . Auth.accountSession) retained)

isolatedConfig :: IO a -> IO a
isolatedConfig action = do
  tmp <- getTemporaryDirectory
  bracket (do (tempPath, h) <- openTempFile tmp "harness-openai-tests"; hClose h; removeFile tempPath; createDirectory tempPath; pure tempPath) removePathForcibly $ \dir ->
    bracket (lookupEnv "XDG_CONFIG_HOME") (\previous -> maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME") previous) $ \_ -> do
      setEnv "XDG_CONFIG_HOME" dir
      action
