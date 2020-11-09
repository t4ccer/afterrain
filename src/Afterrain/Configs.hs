{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Afterrain.Configs where

import           Control.Monad
import qualified Data.ByteString          as B
import           Data.Yaml
import           MyIOLogger
import           System.Directory
import           System.Environment

import           Afterrain.Configs.Hoogle
import           Afterrain.Utils.Loggers

newtype Config = Config
  { hoogleConfig :: HoogleConfig
  }

instance ToJSON Config where
  toJSON conf = object ["hoogle-config" .= hoogleConfig conf]

instance FromJSON (Logger Config) where
  parseJSON (Object v) = do
    hoogle_c :: Logger HoogleConfig <- v .: "hoogle-config"
    return (Config <$> hoogle_c)

defConfig :: Config
defConfig = Config
  { hoogleConfig = defHoogleConfig
  }

configFilePath :: IOLogger String
configFilePath = do
  f <- appendIOLogs ignore (debugLog "Read HOME env var") $ fromIO $ getEnv "HOME"
  return (f++"/.afterrain.yaml")

createConfigFileIfNotExists :: IOLogger ()
createConfigFileIfNotExists = do
  path   <- configFilePath
  exists <- appendIOLogs ignore (debugLog "Checked if config file exists") $ fromIO $ doesFileExist path
  unless exists createConfigFile

createConfigFile :: IOLogger ()
createConfigFile = do
  path   <- configFilePath
  appendIOLogs ignore (debugLog "Created config file") $ fromIO $ B.writeFile path $ encode defConfig

readConfigFile :: IOLogger Config
readConfigFile = do
  path   <- configFilePath
  cont   <- fromIOWithDebugLog ignore "Read config file" $ B.readFile path
  let dec = decodeEither' cont
  case dec of
    Left  e -> failWithIOLogs ignore (errorLog ("Failed parsing config file: " ++ show e))
    Right x -> appendIOLogs ignore (debugLog "Parsed config file") $ liftLogger x
