module Main where

import           System.Environment     (getArgs)
import           Afterrain.Highlighters (highlight)

main :: IO ()
main = do
  cmd   <- unwords <$> getArgs
  input <- getContents
  highlight cmd input
