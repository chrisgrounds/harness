{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client (NetworkEvent (..), ClientTask (..), worker, safeText) where

import Client.Settings
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (forever, void)
import Core
import Core.Protocol (receiveJSON, sendJSON)
import qualified Data.ByteString as B
import Data.Char (isControl)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Socket)
import qualified OpenAI.Auth as Auth
import qualified OpenAI.Http as Http

data NetworkEvent
  = RunChanged Run | TurnDone | Disconnected Text | ServerModel Model | ReplyRate Double
  | ForChat Int NetworkEvent | ServerModels [Model]
  | RunFailed Text
  | SettingsLoaded SettingsSnapshot | SettingsBusy Bool | SettingsFailed Text
  deriving (Eq, Show)

data ClientTask = SendToServer ClientMessage | SignIn Bool | SelectAccount Text | SignOut
  | RefreshSettings | CancelSignIn
  deriving (Eq, Show)

-- Reads continue during sign-in, catalog loading and runs. Each server token
-- request is answered using the chat's account, independent of the active UI.
worker :: Socket -> IO ClientTask -> (NetworkEvent -> IO ()) -> IO ()
worker sock next publish = do
  writes <- newMVar ()
  jobs <- newMVar []
  authJob <- newIORef Nothing
  manager <- Http.newManager
  let send message = withMVar writes (\() -> sendJSON sock message)
      launch action = do
        job <- async action
        modifyMVar_ jobs (pure . (job :))
        pure job
      reportSettings (err :: SomeException) = case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> publish (SettingsFailed (T.pack (displayException err)))
      settings action = do
        previous <- readIORef authJob
        mapM_ cancel previous
        publish (SettingsBusy True)
        job <- launch $ (do
          _ <- action
          loadSettings >>= publish . SettingsLoaded)
          `catch` reportSettings
          `finally` publish (SettingsBusy False)
        writeIORef authJob (Just job)
      event message = case message of
        Connected m -> publish (ServerModel m)
        AvailableModels ms -> publish (ServerModels ms)
        ChatEvent chat item -> case item of
          RunUpdated run -> publish (ForChat chat (RunChanged run))
          TurnFinished -> publish (ForChat chat TurnDone)
          TokenRate r -> publish (ForChat chat (ReplyRate r))
          _ -> pure ()
        ChatFailed chat err -> publish (ForChat chat (RunFailed err))
        TokenNeeded request account -> void $ launch $ do
          result <- try (Auth.accessTokenFor manager account `catch` \(err :: Http.OAuthFailure) -> fail (show err))
          let answer = case result of
                Left (err :: IOException) -> Left (T.pack (displayException err))
                Right token -> Right token
          send (TokenResponse request answer)
        RunUpdated run -> publish (RunChanged run)
        TurnFinished -> publish TurnDone
        TokenRate r -> publish (ReplyRate r)
      receive pending = do
        reply <- receiveJSON sock pending
        case reply of
          Nothing -> fail "Server disconnected."
          Just (message, rest) -> event message >> receive rest
      sending = forever $ do
        task <- next
        case task of
          SendToServer message -> send message
          SignIn fresh -> settings (Auth.login Nothing fresh)
          SelectAccount account -> settings (Auth.useAccount account)
          SignOut -> settings Auth.logout
          RefreshSettings -> settings (pure ())
          CancelSignIn -> readIORef authJob >>= mapM_ cancel
      cleanup = readMVar jobs >>= mapM_ cancel
  (do send ClientHello
      settings (pure ())
      race_ (receive B.empty) sending)
    `catch` \(err :: IOException) -> publish (Disconnected (T.pack (displayException err)))
    `finally` cleanup

safeText :: Text -> Text
safeText = T.replace "\t" "    " . T.filter (\c -> not (isControl c) || c == '\n' || c == '\t')
