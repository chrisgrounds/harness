{-# LANGUAGE OverloadedStrings #-}

-- | Layout primitives shared by the window and feature components.
module UI.Layout (pane, setCursorPos', measure) where

import Control.Exception (bracket_)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import Data.Text (Text)
import DearImGui
import qualified DearImGui.Raw as Raw
import Foreign (with)
import ImGui.Extras (withStyleFloat)
import Theme (vec4)
import Theme.Palette (Colour)
import UI.Widgets (withStyleVec)

setCursorPos' :: ImVec2 -> IO ()
setCursorPos' (ImVec2 px py) = setCursorPosX px >> setCursorPosY py

-- | A child window of the given size and background, without scroll bars
-- unless asked for.
pane :: Text -> ImVec2 -> Maybe Colour -> Bool -> IO a -> IO a
pane name size' fill scrolls contents = do
  let flags' =
        (if isJust fill then ImGuiWindowFlags_None else ImGuiWindowFlags_NoBackground)
          .|. (if scrolls then ImGuiWindowFlags_None else ImGuiWindowFlags_NoScrollbar .|. ImGuiWindowFlags_NoScrollWithMouse)
      painted = maybe id (\c -> bracket_ (with (vec4 c) (Raw.pushStyleColor ImGuiCol_ChildBg)) (Raw.popStyleColor 1)) fill
  painted $ withStyleFloat ImGuiStyleVar_ChildRounding 0 $ withChild name size' False flags' (const contents)

-- | Where prose in the column wraps: under 80 characters a line.
measure :: Float -> Float -> Float
measure left column = left + min column 600
