module Afterrain.Highlighters(
  highlight
) where

import           Afterrain.Highlighters.Hoogle (highlightHoogle)
import           Afterrain.Utils.Colors

highlight :: String -> String -> IO ()
highlight "hoogle" input = printColoredStrings $ highlightHoogle input
highlight cmd _          = error ("Highlighter '" ++ cmd ++ "'not found")
