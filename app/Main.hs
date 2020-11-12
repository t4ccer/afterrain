{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO

import           System.Console.CmdArgs     (cmdArgs)

import           Afterrain.App
import           Afterrain.Configs
import           Afterrain.Highlighters
import           Afterrain.Utils.IO
import           Afterrain.Utils.Parameters

main :: IO ()
main = do
  params <- cmdArgs parameters
  runApp params run

runApp :: Parameters -> RIO App a -> IO a
runApp params inner = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions =
          setLogVerboseFormat True
        $ setLogUseColor      True
        $ setLogUseLoc        (verbosity params == Debug)
        $ setLogMinLevel      (if verbosity params == Debug then LevelDebug else LevelInfo)
        logOptions'

  withLogFunc logOptions $ \logFunc -> do
    let app = App
          { appLogFunc   = logFunc
          , appCLIParams = params
          -- , appConfig    = defConfig
          }
    runRIO app inner

run :: RIO App ()
run = do
  params <- asks appCLIParams

  when (show_params params) $ do
    print params
    logDebug "Printed parameters"

  when (recreate_config params) $ do
    createConfigFile
    logInfo "Recreated config file"
    exitSuccess

  createConfigFileIfNotExists

  config <- readConfigFile
  input  <- fmap (++ "\n") . lines <$> getContents

  case highlighter_mode params of
    Unknown -> do
      logError "Highlighter mode not set"
      exitFailure
    Hoogle  -> do
      mapM_ (printHoogle config) input

