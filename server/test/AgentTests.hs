{-# LANGUAGE OverloadedStrings #-}

module AgentTests (agentTests) where

import Agent (runAgent)
import Control.Concurrent.MVar
import Control.Exception (IOException, finally, try)
import Control.Monad (unless)
import Core
import Data.IORef
import Data.Text (Text)
import System.Timeout (timeout)

agentTests :: IO [String]
agentTests = concat <$> mapM check [("parallel tool reads and update barriers", orderedReads), ("failed tool cancels sibling reads", cancelledReads), ("finished agent makes no further model call", stopsWhenFinished)]
  where
    check (label, action) = do
      result <- try (timeout (5 * 1000000) action >>= maybe (fail "timed out") pure) :: IO (Either IOException ())
      pure [label <> ": " <> show err | Left err <- [result]]

request :: Text -> ToolName -> ToolRequest
request callId name = ToolRequest callId FunctionTool (ToolRequestFunctionParams name "{}")

runRequests :: [ToolRequest] -> (ToolRequest -> IO Text) -> IO Chat
runRequests requests handle = runAgent reply 2 (pure False) handle (Chat [] Mercury3 [])
  where
    reply chat = pure $ addMessage message chat
      where
        message
          | null (messages chat) = (textMessage Assistant "") {toolCalls = requests}
          | otherwise = textMessage Assistant "Done."

stopsWhenFinished :: IO ()
stopsWhenFinished = do
  calls <- newIORef (0 :: Int)
  let reply chat = do
        modifyIORef' calls (+ 1)
        pure (addMessage ((textMessage Assistant "") {toolCalls = [request "set" SetTypes]}) chat)
  final <- runAgent reply 5 (pure True) (const (pure "Accepted.")) (Chat [] Mercury3 [])
  made <- readIORef calls
  unless (made == 1 && length (messages final) == 2) (fail ("expected one model call and a tool result, got " <> show made <> " calls"))

orderedReads :: IO ()
orderedReads = do
  first <- newEmptyMVar
  second <- newEmptyMVar
  phase <- newIORef (0 :: Int)
  let requests = [request "first" ReadFile, request "second" RipGrep, request "change" SetTypes, request "next-change" SetFunctions, request "after" ListFiles]
      handle r = do
        let callId = toolRequestId r
        case callId of
          "first" -> putMVar first () >> readMVar second >> atomicModifyIORef' phase (\n -> (n + 1, ()))
          "second" -> readMVar first >> putMVar second () >> atomicModifyIORef' phase (\n -> (n + 1, ()))
          "change" -> do
            value <- readIORef phase
            unless (value == 2) (fail "update ran before reads finished")
            writeIORef phase 3
          "next-change" -> do
            value <- readIORef phase
            unless (value == 3) (fail "updates ran out of order")
            writeIORef phase 4
          _ -> do
            value <- readIORef phase
            unless (value == 4) (fail "read ran before updates finished")
        pure callId
  final <- runRequests requests handle
  let results = [(toolCallId m, content m) | m <- reverse (messages final), role m == Tool]
  unless (results == [(Just (toolRequestId r), Just (toolRequestId r)) | r <- requests]) $
    fail "tool results lost their request order or call IDs"

cancelledReads :: IO ()
cancelledReads = do
  started <- newEmptyMVar
  cancelled <- newEmptyMVar
  blocked <- newEmptyMVar
  let handle r = case toolRequestFunctionParamsName (toolRequestFunction r) of
        ReadFile -> (putMVar started () >> takeMVar blocked) `finally` putMVar cancelled ()
        _ -> readMVar started >> fail "tool failed"
  result <- try (runRequests [request "blocked" ReadFile, request "failed" ListFiles] handle) :: IO (Either IOException Chat)
  case result of
    Left _ -> takeMVar cancelled
    Right _ -> fail "tool failure was swallowed"
