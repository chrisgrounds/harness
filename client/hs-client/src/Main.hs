{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, bracket, catch, displayException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Parser (json')
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char (isControl, isSpace)
import Data.List (find, intercalate)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Console.Haskeline
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (hIsTerminalDevice, stdout)

data Message = Message T.Text (Maybe T.Text) [ToolRequest]

instance FromJSON Message where
  parseJSON = withObject "message" $ \v ->
    Message <$> v .: "role" <*> v .:? "content" <*> v .:? "tool_calls" .!= []

data ToolRequest = ToolRequest T.Text T.Text

instance FromJSON ToolRequest where
  parseJSON = withObject "tool request" $ \v -> do
    function <- v .: "function"
    ToolRequest <$> function .: "name" <*> function .: "arguments"

newtype Reply = Reply [Message]

instance FromJSON Reply where
  parseJSON = withObject "reply" $ \v -> Reply <$> v .: "messages"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn "Usage: hs-client [SOCKET]\nDefault socket: /tmp/harness.sock\nEnter sends; Up/Down recalls input; /quit or Ctrl-D exits."
    [] -> runClient "/tmp/harness.sock"
    [path] | take 1 path /= "-" -> runClient path
    _ -> die "Usage: hs-client [SOCKET]"

runClient :: FilePath -> IO ()
runClient path = catch session reportError
  where
    reportError :: IOException -> IO ()
    reportError err = die ("hs-client: " ++ displayException err)
    session = do
      terminal <- hIsTerminalDevice stdout
      noColor <- lookupEnv "NO_COLOR"
      term <- lookupEnv "TERM"
      let color = terminal && isNothing noColor && term /= Just "dumb"
      bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        connect sock (SockAddrUnix path)
        runInputT defaultSettings $ handleInterrupt (pure ()) $ withInterrupt $ do
          interactive <- haveTerminalUI
          when interactive $ outputStrLn (paint color "2" "harness · /quit to exit" ++ "\n")
          chat sock color interactive B.empty

chat :: Socket -> Bool -> Bool -> B.ByteString -> InputT IO ()
chat sock color interactive pending = do
  input <- getInputLine (if interactive then paint color "1;36" "you     " ++ "> " else "")
  case input of
    Nothing -> pure ()
    Just line
      | T.strip (T.pack line) == "/quit" -> pure ()
      | all isSpace line -> chat sock color interactive pending
      | otherwise -> do
          unless interactive $ outputStrLn (renderMessage color "1;36" "you" line)
          let payload = L.toStrict $ encode $ object ["role" .= ("user" :: T.Text), "content" .= line]
          -- The current server reads each request with a single recv of 1024 bytes.
          if B.length payload > 1024
            then do
              outputStrLn (paint color "33" "Message too long for the server (1024 encoded bytes maximum).")
              chat sock color interactive pending
            else do
              liftIO $ sendAll sock payload
              (reply, rest) <- liftIO $ receiveReply sock pending
              outputStrLn (renderReply color reply ++ "\n")
              chat sock color interactive rest

-- The server sends an unframed JSON object, with newest messages first.
-- Incremental parsing handles socket boundaries, including split UTF-8 characters.
receiveReply :: Socket -> B.ByteString -> IO (Message, B.ByteString)
receiveReply sock initial = consume (A.parse json' initial)
  where
    consume (A.Fail _ _ err) = ioError (userError ("Invalid server response: " ++ err))
    consume (A.Partial next) = do
      chunk <- recv sock 4096
      if B.null chunk
        then ioError (userError "Server disconnected before a complete reply arrived.")
        else consume (next chunk)
    consume (A.Done rest value) = case fromJSON value of
      Error err -> ioError (userError ("Invalid server response: " ++ err))
      Success (Reply messages) -> case find isAgent messages of
        Nothing -> ioError (userError "Server response contains no agent reply.")
        Just message -> pure (message, rest)
    isAgent (Message role _ _) = role == "assistant"

renderReply :: Bool -> Message -> String
renderReply color (Message _ content requests) = intercalate "\n" (agentLines ++ toolLines)
  where
    agentLines = case content of
      Just text | not (T.null text) -> [renderMessage color "1;32" "agent" (T.unpack text)]
      _ | null requests -> [renderMessage color "1;32" "agent" "(no text reply)"]
      _ -> []
    toolLines =
      [ renderMessage color "33" "tool" (T.unpack (name <> " " <> arguments))
      | ToolRequest name arguments <- requests
      ]

renderMessage :: Bool -> String -> String -> String -> String
renderMessage color code label content =
  paint color code (label ++ replicate (8 - length label) ' ') ++ "> " ++
    T.unpack (T.replace "\n" "\n          " (T.pack (filter safeChar content)))
  where
    safeChar c = not (isControl c) || c == '\n' || c == '\t'

paint :: Bool -> String -> String -> String
paint False _ value = value
paint True code value = "\ESC[" ++ code ++ "m" ++ value ++ "\ESC[0m"
