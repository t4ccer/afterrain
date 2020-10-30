module Afterrain.Utils.Colors(
    ColoredString
  , Color
  , applyColor
  , printColoredString
  , printColoredStrings

  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , grey
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
) where

import           Data.Function ((&))
import           Data.Text     (pack)
import           Rainbow

type Color = Radiant
type ColoredString = Chunk

applyColor :: Color -> String -> ColoredString
applyColor c s = chunk (pack s) & fore c

printColoredString :: Chunk -> IO ()
printColoredString = putChunk

printColoredStrings :: [Chunk] -> IO ()
printColoredStrings = putChunks
