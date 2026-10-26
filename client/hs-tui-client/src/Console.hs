{-# LANGUAGE OverloadedStrings #-}

module Console (runConsole) where

import Client (receiveChat, safeText, userMessage)
import Control.Monad (unless)
import Core
import Core.Protocol (sendJSON)
import qualified Data.ByteString as B
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket (Socket)
import System.IO (isEOF)
import Text.Printf (printf)

runConsole :: Socket -> IO ()
runConsole sock = loop B.empty
  where
    loop pending = do
      done <- isEOF
      unless done $ do
        line <- TIO.getLine
        case T.strip line of
          "/quit" -> pure ()
          "" -> loop pending
          _ -> do
            putStrLn (renderMessage "you" line)
            sendJSON sock (userMessage line)
            (reply, rest) <- receiveChat sock pending
            let turn = reverse (takeWhile ((/= User) . role) (messages reply))
            putStrLn (intercalate "\n" (map renderReply turn) ++ "\n")
            loop rest

renderReply :: Message -> String
renderReply message = intercalate "\n" (body ++ requests ++ speed)
  where
    body = case content message of
      Just value | not (T.null value) -> [renderMessage label value]
      _ | null (toolCalls message) -> [renderMessage label "(no text reply)"]
      _ -> []
    label = case role message of
      User -> "you"
      Assistant -> "agent"
      Tool -> "tool"
      System -> "system"
    requests =
      [ renderMessage "tool" (T.pack (show (toolRequestFunctionParamsName f)) <> " " <> toolRequestFunctionParamsArguments f)
      | call <- toolCalls message,
        let f = toolRequestFunction call
      ]
    speed = [printf "        %.1f tokens/s" rate | Just rate <- [tokensPerSecond message]]

renderMessage :: String -> Text -> String
renderMessage label value =
  label
    ++ replicate (6 - length label) ' '
    ++ "> "
    ++ T.unpack (T.replace "\n" "\n          " (safeText value))
