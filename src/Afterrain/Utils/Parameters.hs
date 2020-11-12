{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Afterrain.Utils.Parameters where

import           System.Console.CmdArgs

data VerbosityLevel =
    Debug
  | Error
  deriving (Show, Data, Typeable, Eq)

data HighlighterMode =
    Unknown
  | Hoogle
  deriving (Show, Data, Typeable)

data Parameters =
  Parameters
  { verbosity        :: VerbosityLevel
  , recreate_config  :: Bool
  , show_params  :: Bool
  , highlighter_mode :: HighlighterMode
  }
  deriving (Show, Data, Typeable)

parameters :: Parameters
parameters = Parameters
  { verbosity        = Error &= name "v" &= name "verbosity"       &= typ "<debug|error>" &= explicit &= help "Default: error"
  , recreate_config  = False &= name "recreate-config" &= explicit &= help "Recreates default config file"
  , show_params  = False &= name "show-params" &= explicit &= help "Shows params before run"
  , highlighter_mode = Unknown &= args &= typ "<hoogle>"
  }
  &= summary "afterrain by t4ccer"
  &= program "aft"
  &= helpArg [explicit, name "help", name "h", name"?", help "Take a guess"]
  &= versionArg [ignore]
