{-# LANGUAGE OverloadedStrings #-}

module Console (runConsole) where

import Client (receiveEvent, receiveGreeting, safeText, statusIcon, stepChanges)
import Control.Monad (unless, when)
import Core
import Core.Protocol (sendJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket (Socket)
import System.IO (isEOF)

-- | The plain transcript: prints the model from the server's greeting, then
-- runs each requirement read from standard input, printing its progress.
runConsole :: FilePath -> Socket -> IO ()
runConsole dir sock = do
  (m, leftover) <- receiveGreeting sock
  TIO.putStrLn (renderLine "holey" ("model " <> modelId m))
  loop leftover
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
            sendJSON sock (StartRun dir (Requirement line))
            rest <- follow Nothing pending
            putStrLn ""
            loop rest
    follow previous pending = do
      (event, rest) <- receiveEvent sock pending
      case event of
        Connected _ -> follow previous rest
        TurnFinished -> pure rest
        TokenRate _ -> follow previous rest
        RunUpdated run -> do
          mapM_ (TIO.putStrLn . renderLine "holey" . ("warning: " <>)) (newItems warnings previous run)
          mapM_ (TIO.putStrLn . renderLine "holey") (newItems history previous run)
          when (maybe False ((/= iteration run) . iteration) previous) $
            TIO.putStrLn (renderLine "holey" ("iteration " <> T.pack (show (iteration run))))
          mapM_ (TIO.putStrLn . renderStep) (stepChanges previous run)
          mapM_ (TIO.putStrLn . renderLine "holey" . changeTag) (newItems touchedFiles previous run)
          follow (Just run) rest
    newItems field previous run = filter (`notElem` maybe [] field previous) (field run)

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
