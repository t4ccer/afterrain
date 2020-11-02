{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Afterrain.Utils.Parameters where

import           System.Console.CmdArgs

data VerbosityLevel =
    Debug
  | Error
  deriving (Show, Data, Typeable)

data HighlighterMode =
    Unknown
  | Hoogle
  deriving (Show, Data, Typeable)

data Parameters =
  Parameters
  { verbosity        :: VerbosityLevel
  , recreate_config  :: Bool
  , validate_params  :: Bool
  , highlighter_mode :: HighlighterMode
  }
  deriving (Show, Data, Typeable)

parameters :: Parameters
parameters = Parameters
  { verbosity        = Error &= name "v" &= name "verbosity"       &= typ "<debug|error>" &= explicit &= help "Default: error"
  , recreate_config  = False &= name "recreate-config" &= explicit &= help "Recreates default config file"
  , validate_params  = False &= name "validate-params" &= explicit &= help "Validates params before run"
  , highlighter_mode = Unknown &= args &= typ "<hoogle>"
  }
  &= summary "afterrain by t4ccer"
  &= program "aft"
  &= helpArg [explicit, name "help", name "h", name"?", help "Take a guess"]
  &= versionArg [ignore]
