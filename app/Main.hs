module Main where

import           System.Environment
import           System.Exit
import           System.Process

import           Afterrain.Highlighters
import           Afterrain.Utils.Colors

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
  let args' = (\x -> "\"" ++ x ++ "\"") $ concatMap (++" ") args
  input <- readProcess cmd [args'] ""
  if cmd == "hoogle"
    then printColoredStrings $ highlightHoogle input
    else putStr input

usage :: IO ()
usage = do
  putStrLn "Usage: aft <command>"
  exitSuccess
