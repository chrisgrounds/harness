{-# LANGUAGE OverloadedStrings #-}

module UI.Sidebar (SidebarView (..), sidebar) where

import Client (safeText)
import Client.Settings (ModelChoice (..))
import Control.Monad (forM, forM_, when)
import Core (Run (..), StepStatus (..), currentStep)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import DearImGui
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign (with)
import ImGui.Extras (handCursor)
import Theme (lifted, mix, raised, seam)
import Theme.Palette
import UI.Appearance
import UI.Draw (colour)
import UI.Fonts
import UI.Icons (Icon (..), icon)
import UI.Layout (gap, pane, setCursorPos')
import UI.State (Action (..), ChatSummary (..), ago, rateText, runSummary)
import UI.Text (ellipsize, label, paragraph, rightAt, textWidth)
import UI.Widgets (Navigation (..), navigationButton)

data SidebarView = SidebarView
  { sidebarTitle :: Text,
    sidebarProject :: Text,
    sidebarSocket :: FilePath,
    sidebarChats :: [ChatSummary],
    sidebarCurrent :: Int,
    -- | When each chat last changed, where known, and the time now.
    sidebarTimes :: Map.Map Int UTCTime,
    sidebarNow :: UTCTime,
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
    gap 4
    setCursorPosX margin
    new <- navigationButton NewChatButton
    footerHeight <- (* 2) <$> withFont (small (fonts e)) getTextLineHeightWithSpacing
    ImVec2 _ listTop <- Raw.getCursorPos
    picked <-
      pane "runs" (ImVec2 width (height - listTop - footerHeight - 72)) Nothing True $
        concat <$> forM (sidebarChats view) (chatRow e view width)
    setCursorPosY (height - footerHeight - 64)
    setCursorPosX margin
    settings <- navigationButton SettingsButton
    gap 10
    withFont (small (fonts e)) $ do
      forM_ (sidebarRate view) $ \rate -> do
        setCursorPosX margin
        label (muted p) (rateText rate)
      setCursorPosX margin
      label (muted p) =<< ellipsize inner (safeText (T.pack (sidebarSocket view)))
    pure (picked ++ [NewChat | new] ++ [OpenSettings | settings])
  where
    p = palette e
    margin = 22

-- | A chat as a card of three lines: its model and how long since it
-- changed, its title, and how its latest run is doing.
chatRow :: Appearance -> SidebarView -> Float -> ChatSummary -> IO [Action]
chatRow e view width summary = do
  smallHeight <- withFont (small (fonts e)) getTextLineHeight
  bodyHeight <- getTextLineHeight
  let chat = summaryChat summary
      inset = 12
      padding = 14
      lineGap = 5
      cardWidth = width - 2 * inset
      textWidth' = cardWidth - 2 * padding
      cardHeight = 2 * padding + 2 * smallHeight + bodyHeight + 2 * lineGap
  setCursorPosX inset
  ImVec2 _ top <- Raw.getCursorPos
  clicked <- invisibleButton ("##chat" <> T.pack (show chat)) (ImVec2 cardWidth cardHeight) ImGuiButtonFlags_None
  hovered <- Raw.isItemHovered
  when hovered handCursor
  ImVec2 x0 y0 <- Raw.getItemRectMin
  let selected' = chat == sidebarCurrent view
  when (selected' || hovered) $ do
    list <- Raw.getWindowDrawList
    fill <- colour 1 (if selected' then lifted p else raised p)
    with (ImVec2 x0 y0) $ \from -> with (ImVec2 (x0 + cardWidth) (y0 + cardHeight)) $ \to -> do
      DrawList.addRectFilled list from to fill 12 ImDrawFlags_None
      when selected' $ do
        edge <- colour 1 (seam p)
        DrawList.addRect list from to edge 12 1 ImDrawFlags_None
  let left = inset + padding
      right = inset + cardWidth - padding
      at offset = setCursorPos' (ImVec2 left (top + offset))
  withFont (small (fonts e)) $ do
    at padding
    let when' = maybe "" (`ago` sidebarNow view) (Map.lookup chat (sidebarTimes view))
    whenWidth <- textWidth when'
    label (muted p) =<< ellipsize (textWidth' - whenWidth - 12) (maybe "No model" (safeText . choiceLabel) (summaryChoice summary))
    rightAt right (muted p) when'
  at (padding + smallHeight + lineGap)
  label (if selected' then foreground p else mix 0.85 (foreground p) (background p)) =<< ellipsize textWidth' (safeText (summaryTitle summary))
  withFont (small (fonts e)) $ do
    at (padding + smallHeight + bodyHeight + 2 * lineGap)
    let run = summaryLatest summary
        (kind, ink) = case currentStep run of
          _ | summaryBusy summary -> (Spinner, accent p)
          Just (_, Failed _) -> (Cross, danger p)
          Nothing | all ((== Passed) . snd) (steps run) -> (Tick, success p)
          _ -> (Hollow, muted p)
    _ <- icon kind ink
    label ink =<< ellipsize (textWidth' - smallHeight - 8) (runSummary run)
  setCursorPos' (ImVec2 inset (top + cardHeight))
  gap 4
  pure [SelectChat chat | clicked]
  where p = palette e
