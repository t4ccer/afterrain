module Afterrain.Highlighters where

import           Afterrain.Configs.Hoogle
import           Afterrain.Highlighters.Hoogle
import           Afterrain.Utils.Colors
import           Afterrain.Utils.Loggers
import           MyIOLogger

highlight :: String -> String -> IOLogger ()
highlight "hoogle" input = do
  strs <- liftLogger $ highlightHoogle defHoogleConfig input
  appendIOLogs ignore [Log Debug "Printed highlighted strings"] $ fromIO $ printColoredStrings strs
highlight cmd _          = failWithIOLogs ignore [Log Error ("No defined highlighter for command: "++cmd)]

