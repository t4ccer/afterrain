{-# LANGUAGE NoImplicitPrelude #-}

module Afterrain.App where

-- import           Afterrain.Configs
import           Afterrain.Utils.Parameters
import           RIO

data App = App
  { appLogFunc   :: !LogFunc
  , appCLIParams :: !Parameters
  -- , appConfig    :: !Config
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
