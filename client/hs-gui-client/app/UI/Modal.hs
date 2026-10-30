{-# LANGUAGE OverloadedStrings #-}

-- | The one modal every dialog is drawn in: a titled card over the dimmed
-- window, dismissed by its cross or Escape, with fields and actions laid
-- out alike in each.
module UI.Modal (Modal (..), modal, modalField, modalActions) where

import Control.Exception (bracket_, finally)
import Control.Monad (unless, when)
import Data.IORef (newIORef, readIORef)
import Data.Text (Text)
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign (with, (.|.))
import ImGui.Extras (beginModal, getFramePadding, handCursor, keyPressed, withStyleFloat)
import Theme (lifted, vec4)
import Theme.Palette
import UI.Appearance
import UI.Draw (colour, line)
import UI.Fonts
import UI.Layout (centreNextWindow, gap, setCursorPos')
import UI.Style (transparent, withStyleColours, withStyleVec)
import UI.Text (label, textWidth, withText)

data Modal = Modal
  { -- | Unique among modals: the popup's ID.
    modalId :: Text,
    modalTitle :: Text,
    -- | The card's width, kept within the display; its height follows
    -- the content.
    modalWidth :: Float
  }

-- | Draws the modal, centred on a display of the given size, for as long
-- as it is called. The body returns its result and whether it is done
-- with the modal, which then closes. Returns the body's result, when
-- drawn, and whether the user dismissed the modal.
modal :: Appearance -> Modal -> ImVec2 -> IO (a, Bool) -> IO (Maybe a, Bool)
modal e m display@(ImVec2 width _) contents = do
  let name = "###" <> modalId m
  opened <- isCurrentPopupOpen name
  unless opened (openPopup name)
  centreNextWindow display ImGuiCond_Always
  with (ImVec2 (min (modalWidth m) (width - 32)) 0) $ \size' -> Raw.setNextWindowSize size' ImGuiCond_Always
  withStyleVec ImGuiStyleVar_WindowPadding (ImVec2 24 20) $
    withStyleFloat ImGuiStyleVar_WindowRounding 14 $
      withStyleFloat ImGuiStyleVar_WindowBorderSize 1 $
        withStyleVec ImGuiStyleVar_FramePadding (ImVec2 12 8) $
          withStyleVec ImGuiStyleVar_ItemSpacing (ImVec2 8 8) $ do
            visible <- withText name (`beginModal` flags')
            if not visible then pure (Nothing, False) else flip finally Raw.endPopup $ do
              crossed <- header e (modalTitle m)
              gap 10
              (result, done) <- contents
              escaped <- keyPressed [] ImGuiKey_Escape
              let dismissed = (crossed || escaped) && not done
              when (done || dismissed) closeCurrentPopup
              pure (Just result, dismissed)
  where
    flags' =
      ImGuiWindowFlags_NoTitleBar .|. ImGuiWindowFlags_NoSavedSettings .|. ImGuiWindowFlags_NoMove
        .|. ImGuiWindowFlags_NoResize .|. ImGuiWindowFlags_AlwaysAutoResize

-- | The title, with a cross at the right end of its line. Returns whether
-- the cross was clicked.
header :: Appearance -> Text -> IO Bool
header e title = do
  ImVec2 available _ <- Raw.getContentRegionAvail
  ImVec2 left top <- Raw.getCursorPos
  lineHeight <- withFont (lead (fonts e)) $ do
    label (foreground p) title
    getTextLineHeight
  let side = 28
      rowHeight = max lineHeight side
  setCursorPos' (ImVec2 (left + available - side + 6) (top + (rowHeight - side) / 2))
  clicked <- invisibleButton "##close-modal" (ImVec2 side side) ImGuiButtonFlags_None
  hovered <- Raw.isItemHovered
  ImVec2 x0 y0 <- Raw.getItemRectMin
  when hovered $ do
    handCursor
    list <- Raw.getWindowDrawList
    fill <- colour 1 (lifted p)
    with (ImVec2 x0 y0) $ \from -> with (ImVec2 (x0 + side) (y0 + side)) $ \to ->
      DrawList.addRectFilled list from to fill 8 ImDrawFlags_None
  ink <- colour 1 (if hovered then foreground p else muted p)
  let c = side / 2
      s = 4.5
  line ink 1.5 (ImVec2 (x0 + c - s) (y0 + c - s)) (ImVec2 (x0 + c + s) (y0 + c + s))
  line ink 1.5 (ImVec2 (x0 + c + s) (y0 + c - s)) (ImVec2 (x0 + c - s) (y0 + c + s))
  setCursorPos' (ImVec2 left (top + rowHeight))
  pure clicked
  where p = palette e

-- | A captioned text field the modal's width, inset below the card and
-- edged in the accent while typed in. It takes the keyboard when the
-- modal opens. Returns the text and whether it changed.
modalField :: Appearance -> Text -> Text -> Text -> IO (Text, Bool)
modalField e name caption draft = do
  withFont (small (fonts e)) $ label (muted p) caption
  ImVec2 available _ <- Raw.getContentRegionAvail
  appearing <- Raw.isWindowAppearing
  when appearing (Raw.setKeyboardFocusHere 0)
  ref <- newIORef draft
  setNextItemWidth available
  let inset = vec4 (background p)
  changed <- withStyleColours [(ImGuiCol_FrameBg, inset), (ImGuiCol_FrameBgHovered, inset), (ImGuiCol_FrameBgActive, inset)] $
    inputText name ref 4096
  active <- Raw.isItemActive
  when active $ do
    ImVec2 x0 y0 <- Raw.getItemRectMin
    ImVec2 w h <- Raw.getItemRectSize
    list <- Raw.getWindowDrawList
    edge <- colour 0.8 (accent p)
    with (ImVec2 x0 y0) $ \from -> with (ImVec2 (x0 + w) (y0 + h)) $ \to ->
      DrawList.addRect list from to edge 7.5 1.5 ImDrawFlags_None
  value <- readIORef ref
  pure (value, changed)
  where p = palette e

-- | The modal's actions at its foot, right-aligned: a quiet secondary
-- beside a filled primary, which may be disabled. Returns whether the
-- primary and the secondary were clicked.
modalActions :: Appearance -> Text -> Bool -> Text -> IO (Bool, Bool)
modalActions e primary enabled secondary = do
  gap 10
  ImVec2 padX _ <- getFramePadding
  primaryWidth <- (+ 2 * padX) <$> textWidth primary
  secondaryWidth <- (+ 2 * padX) <$> textWidth secondary
  ImVec2 available _ <- Raw.getContentRegionAvail
  left <- getCursorPosX
  setCursorPosX (left + available - primaryWidth - secondaryWidth - 8)
  second <- withStyleColours [(ImGuiCol_Button, transparent)] (button secondary)
  pointer
  sameLine
  first <- bracket_ (beginDisabled (not enabled)) Raw.endDisabled $
    withStyleFloat ImGuiStyleVar_FrameBorderSize 0 $
      withStyleColours
        [ (ImGuiCol_Button, vec4 (accent p)),
          (ImGuiCol_ButtonHovered, vec4 (secondaryAccent p)),
          (ImGuiCol_ButtonActive, vec4 (secondaryAccent p)),
          (ImGuiCol_Text, vec4 (background p))
        ]
        (button primary)
  when enabled pointer
  pure (first && enabled, second)
  where
    p = palette e
    pointer = Raw.isItemHovered >>= (`when` handCursor)
