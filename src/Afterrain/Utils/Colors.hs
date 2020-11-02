{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Afterrain.Utils.Colors(
    ColoredString
  , Color(..)
  , applyColor
  , printColoredString
  , printColoredStrings

  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , grey
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
) where

import           Data.Function ((&))
import           Data.Text     (pack, unpack)
import           Data.Word
import           Data.Yaml
import           Rainbow
import qualified Rainbow.Types as R

type ColoredString = Chunk
data Color =
    Color8   {unColor :: Radiant}
  | Color256 {unColor :: Radiant}

instance ToJSON Color where
  toJSON (Color8   (R.Radiant (R.Color a) _)) = String $ pack $ encMaybe  a
  toJSON (Color256 (R.Radiant _ (R.Color b))) = String $ pack $ encMaybe b

instance FromJSON Color where
  parseJSON (String s) = do
    let s' = unpack s
    let c8 = toEnum8 s'
    case c8 of
      Nothing -> do
        let c256  :: Int   = read s'
        let c256' :: Word8 = fromIntegral c256
        return $ Color256 (R.Radiant (R.Color Nothing) (R.Color $ Just c256'))
      Just _  -> return $ Color8   (R.Radiant (R.Color c8) (R.Color Nothing))


toEnum8 :: String -> Maybe R.Enum8
toEnum8 "E0" = Just R.E0
toEnum8 "E1" = Just R.E1
toEnum8 "E2" = Just R.E2
toEnum8 "E3" = Just R.E3
toEnum8 "E4" = Just R.E4
toEnum8 "E5" = Just R.E5
toEnum8 "E6" = Just R.E6
toEnum8 "E7" = Just R.E7
toEnum8 _    = Nothing

encMaybe :: Show a => Maybe a -> String
encMaybe Nothing  = "null"
encMaybe (Just a) = show a

applyColor :: String -> Color -> ColoredString
applyColor s c = chunk (pack s) & fore (unColor c)

printColoredString :: ColoredString -> IO ()
printColoredString = putChunk

printColoredStrings :: [ColoredString] -> IO ()
printColoredStrings = putChunks
