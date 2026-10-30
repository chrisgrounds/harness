{-# LANGUAGE OverloadedStrings #-}

module UI.Sidebar (SidebarView (..), sidebar) where

import Client (safeText)
import Client.Settings (ModelChoice)
import Control.Exception (bracket_)
import Control.Monad (forM, forM_, when)
import Data.Text (Text)
import qualified Data.Text as T
import DearImGui
import qualified DearImGui.Raw as Raw
import Foreign (with)
import Theme.Palette
import UI.Appearance
import UI.Fonts
import UI.Layout (pane)
import UI.State (Action (..), rateText)
import UI.Widgets

data SidebarView = SidebarView
  { sidebarTitle :: Text,
    sidebarProject :: Text,
    sidebarSocket :: FilePath,
    sidebarChats :: [(Int, Text, Maybe ModelChoice, Bool)],
    sidebarCurrent :: Int,
    sidebarRate :: Maybe Double
  }

-- | The project, chat navigation and settings, in a resizable sidebar.
sidebar :: Appearance -> SidebarView -> ImVec2 -> IO [Action]
sidebar e view size'@(ImVec2 width height) =
  pane "sidebar" size' (Just (background p)) False $ do
    let inner = width - 2 * margin
    gap 26
    setCursorPosX margin
    withFont (heading (fonts e)) $ paragraph (margin + inner) (foreground p) (sidebarTitle view)
    setCursorPosX margin
    withFont (small (fonts e)) $ label (muted p) =<< ellipsize inner (safeText (sidebarProject view))
    gap 16
    setCursorPosX margin
    new <- navigationButton NewChatButton
    gap 12
    footerHeight <- (* 2) <$> withFont (small (fonts e)) getTextLineHeightWithSpacing
    ImVec2 _ listTop <- Raw.getCursorPos
    picked <-
      pane "runs" (ImVec2 width (height - listTop - footerHeight - 72)) Nothing True $
        concat <$> forM (sidebarChats view) (chatRow e (sidebarCurrent view) width)
    setCursorPosY (height - footerHeight - 64)
    setCursorPosX margin
    settings <- navigationButton SettingsButton
    gap 10
    withFont (small (fonts e)) $ do
      setCursorPosX margin
      label (muted p) (maybe "No replies yet" rateText (sidebarRate view))
      setCursorPosX margin
      label (muted p) =<< ellipsize inner (safeText (T.pack (sidebarSocket view)))
    pure (picked ++ [NewChat | new] ++ [OpenSettings | settings])
  where
    p = palette e
    margin = 22

-- | Chat navigation shows titles and running state.
chatRow :: Appearance -> Int -> Float -> (Int, Text, Maybe ModelChoice, Bool) -> IO [Action]
chatRow e current width (chat, title, _, busy) = do
  setCursorPosX 12
  shown <- ellipsize (width - 36) (safeText title)
  let clear = ImVec4 0 0 0 0
      colours = [ImGuiCol_Header, ImGuiCol_HeaderHovered, ImGuiCol_HeaderActive]
      push = forM_ colours (\key -> with clear (Raw.pushStyleColor key))
  clicked <- bracket_ push (Raw.popStyleColor 3) $
    selectableWith defSelectableOptions {selected = chat == current} (shown <> "##chat" <> T.pack (show chat))
  when busy $ do
    setCursorPosX 22
    withFont (small (fonts e)) $ label (accent p) "Running"
  gap 10
  pure [SelectChat chat | clicked]
  where p = palette e
