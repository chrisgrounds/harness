{-# LANGUAGE OverloadedStrings #-}

-- | Layout primitives shared by the window and feature components.
module UI.Layout (pane, setCursorPos', gap, centreNextWindow, measure) where

import Data.Bits ((.|.))
import Data.Maybe (isJust)
import Data.Text (Text)
import DearImGui
import qualified DearImGui.Raw as Raw
import Foreign (with)
import ImGui.Extras (withStyleFloat)
import Theme (vec4)
import Theme.Palette (Colour)
import UI.Style (withStyleColours)

setCursorPos' :: ImVec2 -> IO ()
setCursorPos' (ImVec2 px py) = setCursorPosX px >> setCursorPosY py

-- | Empty space of the given height.
gap :: Float -> IO ()
gap h = with (ImVec2 0 h) Raw.dummy

-- | Centres the next window on the display of the given size, under the
-- condition.
centreNextWindow :: ImVec2 -> ImGuiCond -> IO ()
centreNextWindow (ImVec2 width height) cond =
  with (ImVec2 (width / 2) (height / 2)) $ \centre ->
    with (ImVec2 0.5 0.5) $ \pivot -> Raw.setNextWindowPos centre cond (Just pivot)

-- | A child window of the given size and background, without scroll bars
-- unless asked for.
pane :: Text -> ImVec2 -> Maybe Colour -> Bool -> IO a -> IO a
pane name size' fill scrolls contents = do
  let flags' =
        (if isJust fill then ImGuiWindowFlags_None else ImGuiWindowFlags_NoBackground)
          .|. (if scrolls then ImGuiWindowFlags_None else ImGuiWindowFlags_NoScrollbar .|. ImGuiWindowFlags_NoScrollWithMouse)
      painted = maybe id (\c -> withStyleColours [(ImGuiCol_ChildBg, vec4 c)]) fill
  painted $ withStyleFloat ImGuiStyleVar_ChildRounding 0 $ withChild name size' False flags' (const contents)

-- | Where prose in the column wraps: under 100 characters a line.
measure :: Float -> Float -> Float
measure left column = left + min column 880
