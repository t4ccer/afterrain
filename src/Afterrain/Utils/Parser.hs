module Afterrain.Utils.Parser where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

word :: Parser String
word = many (anySingleBut ' ')

line :: Parser String
line = many (anySingleBut '\n')

mergeL :: [Parser [a]] -> Parser [a]
mergeL = fmap concat . sequence

toL :: Parser a -> Parser [a]
toL = fmap (:[])

merge :: [Parser a] -> Parser [a]
merge = mergeL . map toL

ws :: Parser String
ws = many (char ' ')