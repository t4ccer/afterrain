module Afterrain.Highlighters(
  highlight
) where

import           Afterrain.Highlighters.Hoogle
import           Afterrain.Utils.Colors

highlight :: String -> String -> IO ()
highlight "hoogle" input = printColoredStrings $ highlightHoogle defHoogleConfig input
highlight cmd _          = error ("Highlighter '" ++ cmd ++ "'not found")

defHoogleConfig :: HoogleConfig
defHoogleConfig = HoogleConfig
  { typeColor8        = blue
  , typeColor256      = brightBlue
  , typeConstColor8   = magenta
  , typeConstColor256 = brightMagenta
  , symbolsColor8     = white
  , symbolsColor256   = white
  , functionColor8    = green
  , functionColor256  = brightGreen
  , packageColor8     = green
  , packageColor256   = green
  , commentColor8     = white
  , commentColor256   = grey
  , keywordColor8     = white
  , keywordColor256   = grey
  , queryColor8       = white
  , queryColor256     = grey
  , newlineColor8     = white
  , newlineColor256   = white
  }
