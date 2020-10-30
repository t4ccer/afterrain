module Main where

import           System.Environment
import           System.Exit
import           System.Process

import           Afterrain.Highlighters
import           Afterrain.Utils.Colors

main :: IO ()
main = do
  (cmd:rest) <- getArgs
  if cmd == "-h"
    then do
      putStrLn "Usage: aft <command>"
      exitSuccess
    else do
      input <- readProcess cmd rest ""
      if cmd == "hoogle"
        then printColoredStrings $ highlightHoogle input
        else putStr input
