{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Afterrain.Configs where

import           RIO

import qualified Data.ByteString          as B
import           Data.Yaml
import           System.Directory

import           Afterrain.App
import           Afterrain.Configs.Hoogle
import           Afterrain.Utils.IO

newtype Config = Config
  { hoogleConfig :: HoogleConfig
  }

instance ToJSON Config where
  toJSON conf = object ["hoogle-config" .= hoogleConfig conf]

instance FromJSON Config where
  parseJSON (Object v) = do
    hoogle_c :: HoogleConfig <- v .: "hoogle-config"
    return $ Config hoogle_c
  parseJSON _ = undefined

defConfig :: Config
defConfig = Config
  { hoogleConfig = defHoogleConfig
  }

configFilePath :: RIO App String
configFilePath = do
  f <- getEnv "HOME"
  let path = f++"/.afterrain.yaml"
  logDebug $ mkLog' "Read config file path" path
  return path

createConfigFileIfNotExists :: RIO App ()
createConfigFileIfNotExists = do
  path   <- configFilePath
  exists <- liftIO $ doesFileExist path
  unless exists do
    logDebug "Config file do not exists"
    createConfigFile

createConfigFile :: RIO App ()
createConfigFile = do
  path   <- configFilePath
  liftIO $ B.writeFile path $ encode defConfig
  logDebug $ mkLog' "Created config file" path

readConfigFile :: RIO App Config
readConfigFile = do
  path   <- configFilePath
  cont   <- liftIO $ B.readFile path
  let dec = decodeEither' cont
  case dec of
    Left e -> do
      logError $ mkLog' "Failed parsing config file" e
      exitFailure
    Right x -> do
      logDebug "Parsed config file"
      return x

