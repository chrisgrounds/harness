{-# LANGUAGE OverloadedStrings #-}

module UI.Tools (toolsWindow) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Bits ((.|.))
import DearImGui
import qualified DearImGui.Raw as Raw
import Foreign (peek, with)
import Foreign.C.Types (CBool)
import Theme.Palette
import UI.Appearance
import UI.Fonts
import UI.State (ChatAction (..), allToolNames)
import UI.Widgets

-- | The window '/tools' opens, centred on the display: every tool the
-- model can call.
toolsWindow :: Appearance -> ImVec2 -> IO [ChatAction]
toolsWindow e (ImVec2 width height) = do
  with (ImVec2 (width / 2) (height / 2)) $ \centre ->
    with (ImVec2 0.5 0.5) $ \pivot -> Raw.setNextWindowPos centre ImGuiCond_Appearing (Just pivot)
  with (1 :: CBool) $ \open -> do
    visible <-
      withStyleVec ImGuiStyleVar_WindowPadding (ImVec2 22 18) $
        withText "Tools the model can call" $ \name -> Raw.begin name (Just open) (Just windowFlags)
    flip finally Raw.end $
      when visible $ withFont (mono (fonts e)) $ mapM_ (label (foreground p)) allToolNames
    stillOpen <- peek open
    pure [CloseTools | stillOpen == 0]
  where
    p = palette e
    windowFlags = ImGuiWindowFlags_AlwaysAutoResize .|. ImGuiWindowFlags_NoCollapse .|. ImGuiWindowFlags_NoSavedSettings
