{-# LANGUAGE OverloadedStrings #-}

module UI.Sidebar (SidebarView (..), sidebar, renameModal) where

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
import Data.IORef (newIORef, readIORef, writeIORef)
import UI.State (RenameEditor (..))
import UI.Style (withColourOf)
import UI.Modal (Modal (..), modal, modalActions, modalField)
import ImGui.Extras (keyPressed)

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

-- | A chat as a card of two lines: its model, how its latest run is doing
-- and how long since it changed, then its title.
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
      cardHeight = 2 * padding + smallHeight + bodyHeight + lineGap
  setCursorPosX inset
  ImVec2 _ top <- Raw.getCursorPos
  clicked <- invisibleButton ("##chat" <> T.pack (show chat)) (ImVec2 cardWidth cardHeight) ImGuiButtonFlags_None
  hovered <- Raw.isItemHovered
  ImVec2 x0 y0 <- Raw.getItemRectMin
  when hovered handCursor
  actionsRef <- newIORef []
  itemContextPopup $ do
    rename <- selectable "Rename"
    when rename $ do
      writeIORef actionsRef [BeginRename chat]
      closeCurrentPopup
    deleted <- withColourOf (danger p) $ selectable "Delete"
    when deleted $ do
      writeIORef actionsRef [DeleteChat chat]
      closeCurrentPopup
    separator
    label (foreground p) ("Chat " <> T.pack (show chat))
    withFont (small (fonts e)) $ label (muted p) ("~/.config/harness/logs/chat-" <> T.pack (show chat) <> ".log")
  actions <- readIORef actionsRef
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
        run = summaryLatest summary
        (kind, ink) = case currentStep run of
          _ | summaryBusy summary -> (Spinner, accent p)
          Just (_, Failed _) -> (Cross, danger p)
          Nothing | all ((== Passed) . snd) (steps run) -> (Tick, success p)
          _ -> (Hollow, muted p)
        model = maybe "No model" (fst . T.breakOn " · " . safeText . choiceLabel) (summaryChoice summary)
    whenWidth <- textWidth when'
    _ <- icon kind ink
    label (muted p) . (<> " ·") =<< ellipsize (textWidth' / 2) model
    Raw.sameLine
    statusLeft <- getCursorPosX
    label ink =<< ellipsize (right - whenWidth - 12 - statusLeft) (runSummary run)
    rightAt right (muted p) when'
  at (padding + smallHeight + lineGap)
  label (if selected' then foreground p else mix 0.85 (foreground p) (background p)) =<< ellipsize textWidth' (safeText (summaryTitle summary))
  setCursorPos' (ImVec2 inset (top + cardHeight))
  gap 4
  pure (actions ++ [SelectChat chat | clicked && null actions])
  where p = palette e

-- | The chat's title, edited in a modal; Enter saves it unless blank.
renameModal :: Appearance -> Maybe RenameEditor -> ImVec2 -> IO [Action]
renameModal _ Nothing _ = pure []
renameModal e (Just editor) display = do
  let dialog = Modal ("rename-chat-" <> T.pack (show (renameChatId editor))) "Rename chat" 420
  (contents, dismissed) <- modal e dialog display $ do
    (value, changed) <- modalField e "##rename-title" "Title" (renameDraft editor)
    let blank = T.null (T.strip value)
    (save, cancel) <- modalActions e "Save" (not blank) "Cancel"
    entered <- keyPressed [] ImGuiKey_Enter
    let saved = (save || entered) && not blank
    pure ([EditRename value | changed] ++ [SaveRename | saved] ++ [CancelRename | cancel], saved || cancel)
  pure (concat contents ++ [CancelRename | dismissed])
