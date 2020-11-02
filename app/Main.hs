module Main where

import           System.Environment     (getArgs)
import           Afterrain.Highlighters
import           Afterrain.Utils.Loggers
import           Afterrain.Configs

main :: IO ()
main = do
  cmd   <- unwords <$> getArgs
  input <- getContents
  (_, w) <- runIOLogger $ highlight cmd input
  printErrorLogs w
