module Main where

import           System.Environment
import           System.Exit
import           System.Process

import           Afterrain.Highlighters
import           Afterrain.Highlighters.Hoogle
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
  input <- readProcess cmd args ""
  if cmd == "hoogle"
    then printColoredStrings $ highlightHoogle input
    else putStr input

usage :: IO ()
usage = do
  putStrLn "Usage: aft <command>"
  exitSuccess

test :: IO ()
test = do
  input <- readProcess "hoogle" ["concat"] ""
  print input
  putStrLn "Tokens:"
  mapM_ (mapM_ (putStrLn . show)) $ runParsers input
  putStrLn "Parsed:"
  printColoredStrings $ highlightHoogle input

testLine :: String -> IO ()
testLine = printColoredStrings . highlightHoogle