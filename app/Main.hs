module Main where

import           System.Environment     (getArgs)
import           System.Exit            (exitSuccess)
import           System.Process         (readProcess)

import           Afterrain.Highlighters (highlightHoogle)
import           Afterrain.Utils.Colors (printColoredStrings)

main :: IO ()
main = do
  input <- concatMap (++" ") <$> getArgs
  run input

run :: String -> IO ()
run ""       = usage
run "-h"     = usage
run "--help" = usage
run str      = do
  let (cmd:args) = words str
  let args' = fmap (\x -> "\"" ++ x ++ "\"") args
  input <- readProcess cmd args' ""
  if cmd == "hoogle"
    then printColoredStrings $ highlightHoogle input
    else putStr input

usage :: IO ()
usage = do
  putStrLn "Usage: aft <command>"
  exitSuccess
