{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Afterrain.Configs.Hoogle where

import           Afterrain.Utils.Colors
import           Afterrain.Utils.Loggers
import           Data.Yaml
import           GHC.Generics            (Generic)


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
  , unknownColor8     :: Color
  , unknownColor256   :: Color
  }
  deriving(Generic)

instance ToJSON HoogleConfig where
  toJSON conf = object
    [ "type-color8"         .= typeColor8 conf
    , "type-color256"       .= typeColor256 conf
    , "type-const-color8"   .= typeConstColor8 conf
    , "type-const-color256" .= typeConstColor256 conf
    , "symbols-color8"      .= symbolsColor8 conf
    , "symbols-color256"    .= symbolsColor256 conf
    , "function-color8"     .= functionColor8 conf
    , "function-color256"   .= functionColor256 conf
    , "package-color8"      .= packageColor8 conf
    , "package-color256"    .= packageColor256 conf
    , "comment-color8"      .= commentColor8 conf
    , "comment-color256"    .= commentColor256 conf
    , "keyword-color8"      .= keywordColor8 conf
    , "keyword-color256"    .= keywordColor256 conf
    , "query-color8"        .= queryColor8 conf
    , "query-color256"      .= queryColor256 conf
    , "unknown-color8"      .= unknownColor8 conf
    , "unknown-color256"    .= unknownColor256 conf
    ]
instance FromJSON (Logger HoogleConfig) where
  parseJSON (Object v) = do
    typeColor8'        <- v .: "type-color8"
    typeColor256'      <- v .: "type-color256"
    typeConstColor8'   <- v .: "type-const-color8"
    typeConstColor256' <- v .: "type-const-color256"
    symbolsColor8'     <- v .: "symbols-color8"
    symbolsColor256'   <- v .: "symbols-color256"
    functionColor8'    <- v .: "function-color8"
    functionColor256'  <- v .: "function-color256"
    packageColor8'     <- v .: "package-color8"
    packageColor256'   <- v .: "package-color256"
    commentColor8'     <- v .: "comment-color8"
    commentColor256'   <- v .: "comment-color256"
    keywordColor8'     <- v .: "keyword-color8"
    keywordColor256'   <- v .: "keyword-color256"
    queryColor8'       <- v .: "query-color8"
    queryColor256'     <- v .: "query-color256"
    unknownColor8'     <- v .: "unknown-color8"
    unknownColor256'   <- v .: "unknown-color256"
    return $ do
        typeColor8''        <- typeColor8'
        typeColor256''      <- typeColor256'
        typeConstColor8''   <- typeConstColor8'
        typeConstColor256'' <- typeConstColor256'
        symbolsColor8''     <- symbolsColor8'
        symbolsColor256''   <- symbolsColor256'
        functionColor8''    <- functionColor8'
        functionColor256''  <- functionColor256'
        packageColor8''     <- packageColor8'
        packageColor256''   <- packageColor256'
        commentColor8''     <- commentColor8'
        commentColor256''   <- commentColor256'
        keywordColor8''     <- keywordColor8'
        keywordColor256''   <- keywordColor256'
        queryColor8''       <- queryColor8'
        queryColor256''     <- queryColor256'
        unknownColor8''     <- unknownColor8'
        unknownColor256''   <- unknownColor256'

        return HoogleConfig
          { typeColor8        = typeColor8''
          , typeColor256      = typeColor256''
          , typeConstColor8   = typeConstColor8''
          , typeConstColor256 = typeConstColor256''
          , symbolsColor8     = symbolsColor8''
          , symbolsColor256   = symbolsColor256''
          , functionColor8    = functionColor8''
          , functionColor256  = functionColor256''
          , packageColor8     = packageColor8''
          , packageColor256   = packageColor256''
          , commentColor8     = commentColor8''
          , commentColor256   = commentColor256''
          , keywordColor8     = keywordColor8''
          , keywordColor256   = keywordColor256''
          , queryColor8       = queryColor8''
          , queryColor256     = queryColor256''
          , unknownColor8     = unknownColor8''
          , unknownColor256   = unknownColor256''
          }


defHoogleConfig :: HoogleConfig
defHoogleConfig = HoogleConfig
  { typeColor8        = Color8   blue
  , typeColor256      = Color256 brightBlue
  , typeConstColor8   = Color8   red
  , typeConstColor256 = Color256 brightRed
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
  , unknownColor8     = Color8   white
  , unknownColor256   = Color256 white
  }
