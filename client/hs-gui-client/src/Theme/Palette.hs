{-# LANGUAGE OverloadedStrings #-}

-- | The theme's colours, read from a TOML file of @name = [r, g, b]@ entries.
module Theme.Palette (Colour (..), Palette (..), parsePalette) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Toml (Table, Table' (..), Value, Value' (..), forgetTableAnns, parse)

-- | A colour with 0-255 components.
data Colour = Colour {red, green, blue :: Int}
  deriving (Eq, Show)

-- | Each colour by what it is used for; the TOML names are in brackets.
data Palette = Palette
  { -- | The window (@background@).
    background :: Colour,
    -- | Body text (@text@).
    foreground :: Colour,
    -- | Holey, the current step and buttons (@orange@).
    accent :: Colour,
    -- | Hovered buttons and the tools (@secondaryOrange@).
    secondaryAccent :: Colour,
    -- | The user's messages and an active scroll bar (@cyan@).
    highlight :: Colour,
    -- | Secondary text (@muted@).
    muted :: Colour,
    -- | Borders and metadata (@border@).
    border :: Colour,
    -- | Failures (@error@).
    danger :: Colour,
    -- | Passed steps and the ready status (@success@).
    success :: Colour
  }
  deriving (Eq, Show)

-- | Reads a palette, naming the first missing or malformed colour.
parsePalette :: Text -> Either String Palette
parsePalette source = do
  table <- forgetTableAnns <$> parse source
  let colour = colourIn table
  Palette
    <$> colour "background"
    <*> colour "text"
    <*> colour "orange"
    <*> colour "secondaryOrange"
    <*> colour "cyan"
    <*> colour "muted"
    <*> colour "border"
    <*> colour "error"
    <*> colour "success"

colourIn :: Table -> Text -> Either String Colour
colourIn (MkTable entries) name = case snd <$> Map.lookup name entries of
  Nothing -> Left ("missing colour " ++ T.unpack name)
  Just value -> maybe (Left (T.unpack name ++ " must be an [r, g, b] array of 0-255")) Right (rgb value)

rgb :: Value -> Maybe Colour
rgb (List' _ [Integer' _ r, Integer' _ g, Integer' _ b])
  | all (\c -> c >= 0 && c <= 255) [r, g, b] = Just (Colour (fromInteger r) (fromInteger g) (fromInteger b))
rgb _ = Nothing
