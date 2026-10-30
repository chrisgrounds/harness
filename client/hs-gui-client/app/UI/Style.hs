{-# LANGUAGE PatternSynonyms #-}

-- | Dear ImGui style changes scoped to an action, popped however it ends.
module UI.Style
  ( withStyleVec,
    withStyleColours,
    withColourOf,
    transparent,
  )
where

import Control.Exception (bracket_)
import Control.Monad (forM_)
import DearImGui (ImGuiCol, ImGuiStyleVar, ImVec2, ImVec4 (..), pattern ImGuiCol_Text)
import qualified DearImGui.Raw as Raw
import Foreign (with)
import Theme (vec4)
import Theme.Palette (Colour)

withStyleVec :: ImGuiStyleVar -> ImVec2 -> IO a -> IO a
withStyleVec var value = bracket_ (with value (Raw.pushStyleVar var)) (Raw.popStyleVar 1)

-- | The style colours set for the action.
withStyleColours :: [(ImGuiCol, ImVec4)] -> IO a -> IO a
withStyleColours colours =
  bracket_
    (forM_ colours $ \(col, value) -> with value (Raw.pushStyleColor col))
    (Raw.popStyleColor (fromIntegral (length colours)))

-- | Text drawn in the colour.
withColourOf :: Colour -> IO a -> IO a
withColourOf c = withStyleColours [(ImGuiCol_Text, vec4 c)]

-- | No colour at all, for parts of a widget that are not to be drawn.
transparent :: ImVec4
transparent = ImVec4 0 0 0 0
