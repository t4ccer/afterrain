{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Afterrain.Utils.Parameters where

import           Data.Char                  (toLower)
import           Generics.Deriving.ConNames
import           GHC.Generics
import           System.Console.CmdArgs hiding (modes)

data VerbosityLevel =
    Debug
  | Error
  deriving (Show, Data, Typeable, Eq)

data HighlighterMode =
    Unknown
  | Hoogle
  deriving (Show, Data, Typeable, Generic)

data Parameters =
  Parameters
  { verbosity        :: VerbosityLevel
  , recreate_config  :: Bool
  , show_params      :: Bool
  , highlighter_mode :: HighlighterMode
  }
  deriving (Show, Data, Typeable)

-- | Auto generates help info for highlighter modes
modes :: String
modes = (\x -> "<" ++ x ++ ">") $ concat $ init $ concatMap ((\x -> [x, "|"]) . map toLower) $ tail $ conNames Unknown

parameters :: Parameters
parameters = Parameters
  { verbosity        = Error &= name "v" &= name "verbosity"       &= typ "<debug|error>" &= explicit &= help "Default: error"
  , recreate_config  = False &= name "recreate-config" &= explicit &= help "Recreates default config file"
  , show_params  = False &= name "show-params" &= explicit &= help "Shows params before run"
  , highlighter_mode = Unknown &= args &= typ modes
  }
  &= summary "afterrain by t4ccer"
  &= program "aft"
  &= helpArg [explicit, name "help", name "h", name"?", help "Take a guess"]
  &= versionArg [ignore]
