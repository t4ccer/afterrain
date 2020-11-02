{-# LANGUAGE BlockArguments #-}
module Main where

import           Control.Monad
import           MyIOLogger
import           System.Console.CmdArgs     (cmdArgs)

import           Afterrain.Configs
import           Afterrain.Highlighters
import           Afterrain.Utils.Loggers
import           Afterrain.Utils.Parameters

main :: IO ()
main = do
  (_, w) <- runIOLogger run
  params <- cmdArgs parameters
  case verbosity params of
    Afterrain.Utils.Parameters.Debug -> printLogs w
    Afterrain.Utils.Parameters.Error -> printErrorLogs w

run :: IOLogger ()
run = do
  params <- fromIOWithDebugLog ignore "Parsed cli parameters" $ cmdArgs parameters
  input  <- fromIOWithDebugLog ignore "Read stdin input" getContents

  when (validate_params params) $ fromIOWithDebugLog ignore "Printed parameters" $ print params
  when (recreate_config params) createConfigFile

  createConfigFileIfNotExists

  case highlighter_mode params of
        Unknown -> failWithIOLogs ignore (errorLog "Highlighter mode not set")
        Hoogle  -> printHoogle input
  