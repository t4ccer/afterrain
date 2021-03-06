{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Afterrain.Configs.Hoogle where

import           Afterrain.Utils.Colors
import           Data.Yaml
import           GHC.Generics            (Generic)

data HoogleConfig = HoogleConfig
  { typeColor8               :: Color
  , typeColor256             :: Color
  , typeConstColor8          :: Color
  , typeConstColor256        :: Color
  , symbolsColor8            :: Color
  , symbolsColor256          :: Color
  , functionColor8           :: Color
  , functionColor256         :: Color
  , packageColor8            :: Color
  , packageColor256          :: Color
  , commentColor8            :: Color
  , commentColor256          :: Color
  , keywordColor8            :: Color
  , keywordColor256          :: Color
  , queryColor8              :: Color
  , queryColor256            :: Color
  , unknownColor8            :: Color
  , unknownColor256          :: Color
  , generateProgressColor8   :: Color
  , generateProgressColor256 :: Color
  , packagesCountColor8      :: Color
  , packagesCountColor256    :: Color
  , generateTimeColor8       :: Color
  , generateTimeColor256     :: Color
  , warningsCountColor8      :: Color
  , warningsCountColor256    :: Color
  , textColor8               :: Color
  , textColor256             :: Color
  }
  deriving(Generic)

instance ToJSON HoogleConfig where
instance FromJSON HoogleConfig where

defHoogleConfig :: HoogleConfig
defHoogleConfig = HoogleConfig
  { typeColor8               = Color8   blue
  , typeColor256             = Color256 brightBlue
  , typeConstColor8          = Color8   red
  , typeConstColor256        = Color256 brightRed
  , symbolsColor8            = Color8   white
  , symbolsColor256          = Color256 white
  , functionColor8           = Color8   green
  , functionColor256         = Color256 brightGreen
  , packageColor8            = Color8   green
  , packageColor256          = Color256 green
  , commentColor8            = Color8   white
  , commentColor256          = Color256 grey
  , keywordColor8            = Color8   white
  , keywordColor256          = Color256 grey
  , queryColor8              = Color8   white
  , queryColor256            = Color256 grey
  , unknownColor8            = Color8   white
  , unknownColor256          = Color256 white
  , generateProgressColor8   = Color8   yellow
  , generateProgressColor256 = Color256 yellow
  , packagesCountColor8      = Color8   grey
  , packagesCountColor256    = Color256 grey
  , generateTimeColor8       = Color8   green
  , generateTimeColor256     = Color256 brightGreen
  , warningsCountColor8      = Color8   red
  , warningsCountColor256    = Color256 red
  , textColor8               = Color8   white
  , textColor256             = Color256 white
  }
