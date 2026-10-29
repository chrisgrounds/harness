{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ChatGPT public-client OAuth. Credentials belong to this app, never Codex.
module OpenAI.Auth
  ( login, logout, status, useAccount, accessToken, tokenSource, accessTokenFor, accountViews, modelCatalog,
    callbackResult, authorizationURL, challenge, validateIdentity,
    Store (..), Account (..), Session (..), TokenReply (..), updatedSession,
    withStore, saveStore, verifyIdentityWithKeys
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async (withAsync, link)
import Control.Exception (IOException, bracket, catch, onException, try, throwIO)
import Control.Lens ((&), (.~))
import Control.Monad (unless, when)
import qualified Crypto.JWT as JWT
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteArray (convert)
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (Query, renderQuery, status200, status400, status404)
import Network.Socket (close)
import Network.Wai (pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket, setBeforeMainLoop)
import qualified OpenAI.Http as Http
import Numeric (showHex)
import System.Directory
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath ((</>))
import System.Info (os)
import System.Exit (ExitCode)
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (setFileMode)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

-- No Show instances: token sets must not appear in diagnostics.
data Session = Session
  { sessionAccess :: Text, sessionRefresh :: Text, sessionIdToken :: Text,
    sessionExpiry :: UTCTime, sessionScopes :: [Text]
  } deriving (Generic)
instance FromJSON Session
instance ToJSON Session

data Account = Account
  { accountClient :: Text, accountSubject :: Text, accountEmail :: Maybe Text,
    accountSession :: Maybe Session
  } deriving (Generic)
instance FromJSON Account
instance ToJSON Account

data Store = Store
  { storeHost :: Text, storeAccounts :: Map.Map Text Account, storeActive :: Maybe Text
  } deriving (Generic)
instance FromJSON Store
instance ToJSON Store

data TokenReply = TokenReply
  { replyAccess :: Text, replyRefresh :: Maybe Text, replyId :: Maybe Text,
    replyLifetime :: NominalDiffTime, replyScopes :: Maybe Text
  }
instance FromJSON TokenReply where
  parseJSON = withObject "token response" $ \o -> do
    kind <- o .: "token_type" :: Parser Text
    unless (T.toLower kind == "bearer") (fail "Expected bearer credentials")
    TokenReply <$> o .: "access_token" <*> o .:? "refresh_token" <*> o .:? "id_token"
      <*> o .: "expires_in" <*> o .:? "scope"

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "harness/openai"

-- | A process-wide and cross-process lock protects rotating refresh tokens.
-- The private directory also protects the lock file and atomic-write temps.
withStore :: (FilePath -> Store -> IO a) -> IO a
withStore action = do
  dir <- configDir
  createDirectoryIfMissing True dir
  setFileMode dir 0o700
  withFileLock (dir </> "session.lock") Exclusive $ \_ -> do
    let file = dir </> "accounts.json"
    exists <- doesFileExist file
    store <- if exists
      then L.readFile file >>= either (const (fail "Invalid OpenAI account store; restore it from backup.")) pure . eitherDecode
      else do
        host <- newHost
        let fresh = Store host Map.empty Nothing
        saveStore file fresh
        pure fresh
    action file store

saveStore :: FilePath -> Store -> IO ()
saveStore file store = do
  dir <- configDir
  (temp, handle) <- openBinaryTempFile dir "accounts.tmp"
  let cleanup = do
        hClose handle `catch` ignoreIO
        removeFile temp `catch` ignoreIO
  (do setFileMode temp 0o600
      L.hPut handle (encode store)
      hClose handle
      renameFile temp file) `onException` cleanup
  where
    ignoreIO :: IOException -> IO ()
    ignoreIO _ = pure ()

newHost :: IO Text
newHost = do
  bytes <- B.unpack <$> (getRandomBytes 16 :: IO B.ByteString)
  let v4 = [if i == 6 then b .&. 15 .|. 64 else if i == 8 then b .&. 63 .|. 128 else b | (i,b) <- zip [0 :: Int ..] bytes]
      hex b = let s = showHex b "" in if length s == 1 then '0':s else s
      raw = concatMap hex v4
      uuid = take 8 raw <> "-" <> take 4 (drop 8 raw) <> "-" <> take 4 (drop 12 raw) <> "-" <> take 4 (drop 16 raw) <> "-" <> drop 20 raw
  pure ("urn:uuid:" <> T.pack uuid)

randomText :: IO Text
randomText = TE.decodeUtf8 . Base64.encodeUnpadded <$> (getRandomBytes 32 :: IO B.ByteString)

challenge :: Text -> Text
challenge verifier = TE.decodeUtf8 (Base64.encodeUnpadded (convert (hash (TE.encodeUtf8 verifier) :: Digest SHA256)))

authorizationURL :: Text -> Maybe Text -> Text -> Text -> Text -> Text -> String
authorizationURL host client redirect state nonce verifier =
  "https://auth.openai.com/api/accounts/authorize" <> BC.unpack (renderQuery True fields)
  where
    fields = map (\(k,v) -> (k, Just (TE.encodeUtf8 v))) $
      [("client_id", fromMaybe "dynamic_agent_client" client), ("ext_agent_host_id", host),
       ("redirect_uri", redirect), ("response_type", "code"),
       ("scope", "openid profile email offline_access resource.invoke chatgpt.tokens.use.direct"),
       ("resource", resource), ("state", state), ("nonce", nonce),
       ("code_challenge_method", "S256"), ("code_challenge", challenge verifier)]
      ++ [("agent_name_hint", "Harness") | not (isJust client)]

resource :: Text
resource = "https://api.openai.com/v1"

-- | Validate state before handling success or denial. Duplicate parameters and
-- a changed returning client ID are rejected rather than replacing an account.
callbackResult :: Text -> Maybe Text -> Query -> Either String (Text, Text)
callbackResult expected existing query = do
  state <- required "state"
  unless (state == expected) (Left "Sign-in state did not match.")
  case lookup "error" query of
    Just _ -> Left "ChatGPT sign-in was declined or failed."
    Nothing -> pure ()
  code <- required "code"
  client <- case existing of
    Nothing -> required "client_id"
    Just saved -> case lookup "client_id" query of
      Nothing -> Right saved
      Just _ -> do returned <- required "client_id"
                   if returned == saved then Right saved else Left "Sign-in returned a different client registration."
  unless (client /= "dynamic_agent_client" && not (T.null client)) (Left "Sign-in did not issue a client ID.")
  pure (code, client)
  where
    required key = case [v | (k, Just v) <- query, k == key] of
      [value] | not (B.null value) -> either (const (Left "Invalid callback encoding.")) Right (TE.decodeUtf8' value)
      _ -> Left "Missing or duplicate sign-in callback parameter."

login :: Maybe Text -> Bool -> IO ()
login selected fresh = do
  store <- withStore (\_ saved -> pure saved)
  let chosen = if fresh then Nothing else selected `orElse` storeActive store
  previous <- traverse (findAccount store) chosen
  state <- randomText
  nonce <- randomText
  verifier <- randomText
  manager <- Http.newManager
  (authCode, issuedClient, redirect) <- bracket openFreePort (close . snd) $ \(port, socket) -> do
    let redirect = "http://127.0.0.1:" <> T.pack (show port) <> "/auth/callback"
        url = authorizationURL (storeHost store) (accountClient <$> previous) redirect state nonce verifier
    result <- newEmptyMVar
    ready <- newEmptyMVar
    let application req respond
          | pathInfo req /= ["auth", "callback"] = respond (responseLBS status404 [] "Not found")
          | lookup "state" (queryString req) /= Just (Just (TE.encodeUtf8 state)) = respond (responseLBS status400 [] "Invalid sign-in state")
          | otherwise = do
              let outcome = callbackResult state (accountClient <$> previous) (queryString req)
              _ <- tryPutMVar result (fmap (\(c, cid) -> (c, cid, redirect)) outcome)
              respond (responseLBS status200 [("Content-Type", "text/plain"), ("Cache-Control", "no-store")] "Return to Harness to finish signing in.")
    withAsync (runSettingsSocket (setBeforeMainLoop (putMVar ready ()) defaultSettings) socket application) $ \worker -> do
      link worker
      takeMVar ready
      putStrLn "Continue with ChatGPT in your browser:"
      putStrLn url
      -- No retained ID token is placed in this URL or printed to the terminal.
      _ <- try (readCreateProcessWithExitCode (proc (if os == "darwin" then "open" else "xdg-open") [url]) "") :: IO (Either IOException (ExitCode, String, String))
      outcome <- timeout (5 * 60 * 1000000) (takeMVar result)
      either fail pure (fromMaybe (Left "ChatGPT sign-in timed out. Run server login openai to try again.") outcome)
  -- This is the only network operation that receives the authorization code.
  reply <- tokenRequest manager [("grant_type", "authorization_code"), ("client_id", issuedClient), ("code", authCode), ("code_verifier", verifier), ("redirect_uri", redirect), ("resource", resource)]
  idToken <- maybe (fail "OpenAI did not return an ID token.") pure (replyId reply)
  (subject, email) <- verifyIdentity manager issuedClient (Just nonce) idToken
  when (maybe False ((/= subject) . accountSubject) previous) (fail "The returned ChatGPT identity differs from the selected account.")
  now <- getCurrentTime
  session <- either fail pure (updatedSession now Nothing reply)
  let account = Account issuedClient subject email (Just session)
  withStore $ \file latest ->
    saveStore file latest {storeAccounts = Map.insert issuedClient account (storeAccounts latest), storeActive = Just issuedClient}
  putStrLn ("Signed in: " <> T.unpack (fromMaybe subject email) <> " (" <> T.unpack issuedClient <> ")")
  unless (planEnabled session) (putStrLn "ChatGPT plan usage was not enabled. Run login again to grant access before using models.")
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Signature verification precedes all identity decisions. Missing required
-- claims are rejected as well as claims with invalid values.
verifyIdentity :: Manager -> Text -> Maybe Text -> Text -> IO (Text, Maybe Text)
verifyIdentity manager client nonce token = do
  keys <- Http.getJSON manager "https://auth.openai.com/.well-known/jwks.json" Nothing :: IO JWT.JWKSet
  verifyIdentityWithKeys keys client nonce token

verifyIdentityWithKeys :: JWT.JWKSet -> Text -> Maybe Text -> Text -> IO (Text, Maybe Text)
verifyIdentityWithKeys keys client nonce token = do
  let settings = JWT.defaultJWTValidationSettings (== fromString (T.unpack client))
        & JWT.issuerPredicate .~ (== "https://auth.openai.com")
        & JWT.algorithms .~ Set.fromList [JWT.RS256, JWT.ES256, JWT.EdDSA]
  verified <- (JWT.runJOSE $ do
    jwt <- JWT.decodeCompact (L.fromStrict (TE.encodeUtf8 token))
    JWT.verifyClaims settings keys (jwt :: JWT.SignedJWT)) :: IO (Either JWT.JWTError JWT.ClaimsSet)
  claims <- case verified of
    Left _ -> fail "OpenAI ID token validation failed."
    Right value -> pure (toJSON (value :: JWT.ClaimsSet))
  either fail pure (validateIdentity client nonce claims)

validateIdentity :: Text -> Maybe Text -> Value -> Either String (Text, Maybe Text)
validateIdentity client expected = parseEither $ withObject "identity" $ \o -> do
  issuer <- o .: "iss" :: Parser Text
  audience <- o .: "aud" :: Parser Value
  _ <- o .: "exp" :: Parser Double
  _ <- o .: "iat" :: Parser Double
  subject <- o .: "sub"
  actualNonce <- o .:? "nonce"
  authorizedParty <- o .:? "azp"
  email <- o .:? "email"
  let audiences = case audience of String x -> [x]; Array xs -> [x | String x <- foldr (:) [] xs]; _ -> []
  unless (issuer == "https://auth.openai.com" && client `elem` audiences && not (T.null subject)) (fail "Invalid OpenAI identity.")
  unless (maybe True (\n -> actualNonce == Just n) expected) (fail "OpenAI ID token nonce did not match.")
  unless (maybe (length audiences == 1) (== client) authorizedParty) (fail "Invalid OpenAI authorized party.")
  pure (subject, email)

updatedSession :: UTCTime -> Maybe Session -> TokenReply -> Either String Session
updatedSession now previous reply = do
  refresh <- maybe (Left "OpenAI did not return a refresh token.") Right (replyRefresh reply `orElse` (sessionRefresh <$> previous))
  idToken <- maybe (Left "OpenAI did not return an ID token.") Right (replyId reply `orElse` (sessionIdToken <$> previous))
  when (T.null (replyAccess reply) || T.null refresh || replyLifetime reply <= 0) (Left "OpenAI returned invalid session credentials.")
  let scopes = maybe (maybe [] sessionScopes previous) T.words (replyScopes reply)
  pure (Session (replyAccess reply) refresh idToken (addUTCTime (replyLifetime reply) now) scopes)
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

planEnabled :: Session -> Bool
planEnabled session = all (`elem` sessionScopes session) ["resource.invoke", "chatgpt.tokens.use.direct"]

tokenRequest :: Manager -> [(B.ByteString, Text)] -> IO TokenReply
tokenRequest manager fields = Http.postForm manager "https://auth.openai.com/api/accounts/oauth/token" [(k, TE.encodeUtf8 v) | (k,v) <- fields] >>= Http.parseJSONBody

findAccount :: Store -> Text -> IO Account
findAccount store name = maybe (fail "Unknown ChatGPT account. Run server accounts openai.") pure (Map.lookup name (storeAccounts store))

activeAccount :: Store -> IO (Text, Account)
activeAccount store = do
  name <- maybe (fail "Sign in first with: server login openai") pure (storeActive store)
  account <- findAccount store name
  pure (name, account)

accessToken :: Manager -> IO Text
accessToken manager = tokenSource manager >>= id

-- Bind a running server to its selected account; changing the active CLI
-- account applies to a new server, never to an in-flight build.
tokenSource :: Manager -> IO (IO Text)
tokenSource manager = withStore $ \_ store -> do
  (name, _) <- activeAccount store
  pure (accessTokenFor manager name)

accessTokenFor :: Manager -> Text -> IO Text
accessTokenFor manager name = withStore $ \file store -> do
  account <- findAccount store name
  session <- maybe (fail "ChatGPT account is signed out. Run server login openai.") pure (accountSession account)
  unless (planEnabled session) (fail "ChatGPT plan usage is not enabled. Run server login openai to grant access.")
  now <- getCurrentTime
  current <- if sessionExpiry session > addUTCTime 60 now then pure session else do
    reply <- tokenRequest manager [("grant_type", "refresh_token"), ("client_id", accountClient account), ("refresh_token", sessionRefresh session), ("resource", resource)]
      `catch` \err@(Http.OAuthFailure statusCode code) -> do
        when (statusCode < 500 && code `elem` ["invalid_grant", "invalid_refresh_token", "token_expired", "refresh_token_expired", "refresh_token_invalidated", "refresh_token_reused"]) $
          saveStore file store {storeAccounts = Map.insert name account {accountSession = Nothing} (storeAccounts store)}
        throwIO err
    -- A replacement ID token must still belong to this registration.
    case replyId reply of
      Nothing -> pure ()
      Just token -> do
        (subject, _) <- verifyIdentity manager (accountClient account) Nothing token
        unless (subject == accountSubject account) (fail "OpenAI refreshed a different identity.")
    refreshed <- either fail pure (updatedSession now (Just session) reply)
    saveStore file store {storeAccounts = Map.insert name account {accountSession = Just refreshed} (storeAccounts store)}
    pure refreshed
  unless (planEnabled current) (fail "ChatGPT plan permission was removed. Run server login openai.")
  pure (sessionAccess current)

status :: IO ()
status = withStore $ \_ store -> do
  if Map.null (storeAccounts store) then putStrLn "No ChatGPT accounts. Run server login openai." else
    mapM_ (\(name, account) -> putStrLn
      ((if storeActive store == Just name then "* " else "  ") <> T.unpack name <> "\t" <> T.unpack (fromMaybe (accountSubject account) (accountEmail account)) <> "\t" <>
       maybe "signed out" (\s -> if planEnabled s then "ChatGPT plan enabled" else "plan usage disabled") (accountSession account))) (Map.toList (storeAccounts store))

useAccount :: Text -> IO ()
useAccount name = withStore $ \file store -> do
  _ <- findAccount store name
  saveStore file store {storeActive = Just name}
  putStrLn ("Active ChatGPT account: " <> T.unpack name)

logout :: IO ()
logout = withStore $ \file store -> do
  (name, account) <- activeAccount store
  manager <- Http.newManager
  revoked <- case accountSession account of
    Nothing -> pure True
    Just session -> do
      outcome <- try $ retryRevocation 2 $ do
        discovery <- Http.getJSON manager "https://auth.openai.com/.well-known/openid-configuration" Nothing
        endpoint <- either fail pure (parseEither (withObject "discovery" (.: "revocation_endpoint")) discovery)
        -- Never send a refresh token to an unexpected origin.
        unless ("https://auth.openai.com/" `T.isPrefixOf` endpoint) (fail "Invalid OpenAI revocation endpoint.")
        _ <- Http.postForm manager (T.unpack endpoint) [("token", TE.encodeUtf8 (sessionRefresh session)), ("token_type_hint", "refresh_token"), ("client_id", TE.encodeUtf8 name)]
        pure ()
      pure (either (const False) (const True) (outcome :: Either IOException ()))
  saveStore file store {storeAccounts = Map.insert name account {accountSession = Nothing} (storeAccounts store)}
  putStrLn "Signed out of ChatGPT in Harness."
  unless revoked (putStrLn "Remote revocation was not confirmed. Disconnect Harness in ChatGPT Settings to end remote access.")

-- Revocation can be retried safely, including an already-revoked token.
retryRevocation :: Int -> IO () -> IO ()
retryRevocation retries action = do
  outcome <- try (try action :: IO (Either Http.OAuthFailure ())) :: IO (Either IOException (Either Http.OAuthFailure ()))
  case outcome of
    Right (Right ()) -> pure ()
    Right (Left err@(Http.OAuthFailure code _))
      | code >= 500 && retries > 0 -> again
      | otherwise -> fail (show err)
    Left err
      | retries > 0 -> again
      | otherwise -> throwIO err
  where
    again = threadDelay 200000 >> retryRevocation (retries - 1) action

-- | Public account metadata only; token records never reach the UI state.
accountViews :: IO [(Text, Text, Bool, Bool)]
accountViews = withStore $ \_ store -> pure
  [(name, fromMaybe (accountSubject account) (accountEmail account),
    maybe False planEnabled (accountSession account), storeActive store == Just name)
   | (name, account) <- Map.toList (storeAccounts store)]

modelCatalog :: IO [(Text, Text)]
modelCatalog = do
  manager <- Http.newManager
  token <- accessToken manager
  body <- Http.getJSON manager "https://api.openai.com/v1/models" (Just token)
  either fail pure (parseEither catalog body)
  where
    catalog = withObject "catalog" $ \o -> do
      entries <- o .: "models" :: Parser [Value]
      concat <$> mapM (withObject "model" $ \m -> do
        visibility <- m .: "visibility" :: Parser Text
        if visibility == "list" then (:[]) <$> ((,) <$> m .: "slug" <*> m .: "display_name") else pure []) entries
