{-# LANGUAGE OverloadedStrings #-}

module Console (runConsole) where

import Client (receiveEvent, safeText, statusIcon, stepChanges)
import Control.Monad (unless, when)
import Core
import Core.Protocol (sendJSON)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket (Socket)
import System.IO (isEOF)

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
            TIO.putStrLn (renderLine "you" line)
            sendJSON sock (Requirement line)
            rest <- follow Nothing pending
            putStrLn ""
            loop rest
    follow previous pending = do
      (event, rest) <- receiveEvent sock pending
      case event of
        TurnFinished -> pure rest
        RunUpdated run -> do
          when (maybe False ((/= iteration run) . iteration) previous) $
            TIO.putStrLn (renderLine "holey" ("iteration " <> T.pack (show (iteration run))))
          mapM_ (TIO.putStrLn . renderStep) (stepChanges previous run)
          follow (Just run) rest

renderStep :: (Step, StepStatus) -> Text
renderStep (step, status) =
  renderLine "holey" $
    "[" <> T.pack (show (fromEnum step + 1)) <> "/" <> T.pack (show (length allSteps)) <> "] "
      <> statusIcon status
      <> " "
      <> stepLabel step
      <> case status of
        Failed reason -> ": " <> reason
        _ -> ""

renderLine :: Text -> Text -> Text
renderLine label value =
  label
    <> T.replicate (6 - T.length label) " "
    <> "> "
    <> T.replace "\n" "\n          " (safeText value)
