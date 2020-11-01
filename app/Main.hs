module Main where

import           System.Environment     (getArgs)
import           Afterrain.Highlighters (highlight)
import           Afterrain.Utils.Loggers

main :: IO ()
main = do
  cmd   <- unwords <$> getArgs
  input <- getContents
  (_, w) <- runIOLogger $ highlight cmd input
  printErrorLogs w
