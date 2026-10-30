{-# LANGUAGE OverloadedStrings #-}

module UI.Settings (SettingsView (..), settingsModal) where

import Client (safeText)
import Client.Settings
import Control.Exception (bracket_)
import Control.Monad (forM, forM_, when)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import DearImGui
import qualified DearImGui.Raw as Raw
import Theme.Palette
import UI.Appearance
import UI.Fonts
import UI.Layout (gap)
import UI.State (Action (..))
import UI.Style (withColourOf)
import UI.Text (label, paragraph, rightAligned, textWidth)
import UI.Modal (Modal (..), modal)

data SettingsView = SettingsView
  { settingsVisible :: Bool,
    settingsSnapshot :: SettingsSnapshot,
    settingsBusy :: Bool,
    settingsError :: Maybe Text
  }

-- | A modal: account actions run in the background so the window keeps drawing
-- while the user completes OAuth in their browser.
settingsModal :: Appearance -> SettingsView -> ImVec2 -> IO [Action]
settingsModal e view display
  | not (settingsVisible view) = pure []
  | otherwise = do
            (contents, dismissed) <- modal e (Modal "user-settings" "User settings" 460) display $ do
              ImVec2 available _ <- Raw.getContentRegionAvail
              left <- getCursorPosX
              let edge = left + available
                  accounts = configuredAccounts (settingsSnapshot view)
                  active = listToMaybe [a | a <- accounts, accountActive a]
                  connected = maybe False accountConfigured active
              withFont (lead (fonts e)) $ label (foreground p) "ChatGPT"
              withFont (small (fonts e)) $ rightAligned (muted p) (if connected then "Connected" else "Not connected")
              gap 8
              accountActions <- bracket_ (beginDisabled (settingsBusy view)) Raw.endDisabled $ do
                picked <- if null accounts then do
                  withFont (small (fonts e)) $ paragraph edge (muted p) "Connect your account to use your ChatGPT plan."
                  pure []
                  else do
                    withFont (small (fonts e)) $ label (muted p) "Account"
                    setNextItemWidth available
                    withCombo "##openai-account" (maybe "Choose an account" accountLabel active) $ \expanded ->
                      if not expanded then pure [] else concat <$> forM accounts (\a -> do
                        selected' <- selectableWith defSelectableOptions {selected = accountActive a}
                          (accountLabel a <> "##" <> accountId a)
                        when selected' closeCurrentPopup
                        pure [ChangeAccount (accountId a) | selected'])
                gap 8
                primary <- if connected then button "Refresh models" else button "Continue with ChatGPT"
                manage <- if null accounts then pure False else do
                  sameLine
                  captionWidth <- textWidth "Manage account"
                  setCursorPosX (edge - captionWidth - 20)
                  button "Manage account"
                when manage (openPopup "##account-actions")
                menu <- withPopup "##account-actions" $ \expanded -> if not expanded then pure [] else do
                  reconnect <- selectable "Reconnect ChatGPT"
                  disconnect <- withColourOf (danger p) (selectable "Sign out")
                  pure ([ConnectOpenAI False | reconnect] ++ [DisconnectOpenAI | disconnect])
                pure (picked ++ [if connected then RefreshModels else ConnectOpenAI False | primary] ++ menu)
              progress <- if settingsBusy view then do
                gap 6
                withFont (small (fonts e)) $ paragraph edge (muted p) "Connecting… Finish sign-in in your browser if prompted."
                cancel <- smallButton "Cancel"
                pure [CancelLogin | cancel]
                else pure []
              forM_ (settingsError view) $ \err -> do
                gap 6
                withFont (small (fonts e)) $ paragraph edge (danger p) (safeText err)
              pure (accountActions ++ progress, False)
            pure (fromMaybe [] contents ++ [CloseSettings | dismissed])
  where p = palette e
