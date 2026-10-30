{-# LANGUAGE OverloadedStrings #-}
module SocketTests (socketTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad (unless)
import Core
import Core.Protocol
import qualified Data.ByteString as B
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket hiding (Socket)
import qualified Socket
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)

socketTests :: IO [String]
socketTests = do
  result <- try (timeout (10 * 1000000) scenario >>= maybe (fail "Socket test timed out") pure) :: IO (Either IOException ())
  pure ["chat socket routing: " <> show err | Left err <- [result]]
  where
    scenario = do
      tmp <- getTemporaryDirectory
      bracket (do (file,h) <- openTempFile tmp "harness-socket-test"; hClose h; removeFile file; createDirectory file; pure file) removePathForcibly $ \dir -> do
        let path = dir </> "server.sock"
        seen <- newIORef []
        let handler chosen source publish _ = do
              token <- maybe (pure "legacy") id source
              modifyIORef' seen (++ [(chosen, token)])
              publish (TokenRate 12)
        withAsync (Socket.serve path Mercury3 [Mercury3] handler) $ \server -> do
          link server
          let waitForSocket = do exists <- doesFileExist path; unless exists (threadDelay 10000 >> waitForSocket)
          waitForSocket
          bracket (socket AF_UNIX Stream defaultProtocol) close $ \client -> do
            connect client (SockAddrUnix path)
            pending <- newIORef B.empty
            let receive = do
                  bytes <- readIORef pending
                  result <- receiveJSON client bytes
                  case result of
                    Nothing -> fail "unexpected disconnect"
                    Just (event, rest) -> writeIORef pending rest >> pure event
                expect wanted = do actual <- receive; unless (actual == wanted) (fail ("Unexpected event: " <> show actual))
            expect (Connected Mercury3)
            sendJSON client ClientHello
            expect (AvailableModels [Mercury3])
            sendJSON client (StartChat 4 (OpenAIModel "first") (Just "account-one") (StartRun dir (Requirement "first")))
            sendJSON client (StartChat 9 (OpenAIModel "second") (Just "account-two") (StartRun dir (Requirement "second")))
            first <- receive
            case first of
              TokenNeeded request "account-one" -> sendJSON client (TokenResponse request (Right "first-access"))
              _ -> fail "first chat used the wrong account"
            expect (ChatEvent 4 (TokenRate 12))
            expect (ChatEvent 4 TurnFinished)
            second <- receive
            case second of
              TokenNeeded request "account-two" -> sendJSON client (TokenResponse request (Right "second-access"))
              _ -> fail "second chat used the wrong account"
            expect (ChatEvent 9 (TokenRate 12))
            expect (ChatEvent 9 TurnFinished)
            sendJSON client (StartChat 11 (OpenAIModel "first") Nothing (StartRun dir (Requirement "missing account")))
            rejected <- receive
            case rejected of
              ChatFailed 11 _ -> pure ()
              _ -> fail "OpenAI chat without a client account was accepted"
            expect (ChatEvent 11 TurnFinished)
            sendJSON client (StartRun dir (Requirement "legacy"))
            expect (TokenRate 12)
            expect TurnFinished
            recorded <- readIORef seen
            unless (recorded == [(OpenAIModel "first", "first-access"), (OpenAIModel "second", "second-access"), (Mercury3, "legacy")]) (fail "chat model or token leaked between requests")
            unless (not ("secret-token" `T.isInfixOf` T.pack (show (TokenResponse 1 (Right "secret-token" :: Either Text Text))))) (fail "token was shown in diagnostics")
