{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Afterrain.Configs.Hoogle where

import           Afterrain.Utils.Colors
import Data.Yaml
import GHC.Generics (Generic)

data HoogleConfig = HoogleConfig
  { typeColor8        :: Color
  , typeColor256      :: Color
  , typeConstColor8   :: Color
  , typeConstColor256 :: Color
  , symbolsColor8     :: Color
  , symbolsColor256   :: Color
  , functionColor8    :: Color
  , functionColor256  :: Color
  , packageColor8     :: Color
  , packageColor256   :: Color
  , commentColor8     :: Color
  , commentColor256   :: Color
  , keywordColor8     :: Color
  , keywordColor256   :: Color
  , queryColor8       :: Color
  , queryColor256     :: Color
  , newlineColor8     :: Color
  , newlineColor256   :: Color
  }
  deriving(Generic)

instance ToJSON HoogleConfig
instance FromJSON HoogleConfig

defHoogleConfig :: HoogleConfig
defHoogleConfig = HoogleConfig
  { typeColor8        = Color8   blue
  , typeColor256      = Color256 brightBlue
  , typeConstColor8   = Color8   magenta
  , typeConstColor256 = Color256 brightMagenta
  , symbolsColor8     = Color8   white
  , symbolsColor256   = Color256 white
  , functionColor8    = Color8   green
  , functionColor256  = Color256 brightGreen
  , packageColor8     = Color8   green
  , packageColor256   = Color256 green
  , commentColor8     = Color8   white
  , commentColor256   = Color256 grey
  , keywordColor8     = Color8   white
  , keywordColor256   = Color256 grey
  , queryColor8       = Color8   white
  , queryColor256     = Color256 grey
  , newlineColor8     = Color8   white
  , newlineColor256   = Color256 white
  }