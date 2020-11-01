{-# LANGUAGE MultiWayIf #-}
module Afterrain.Highlighters.Hoogle (highlightHoogle) where

import           Data.Char
import           Data.Either            (fromRight)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Afterrain.Utils.Colors
import           Afterrain.Utils.Parser

data HoogleToken =
    Type      String
  | TypeConst String
  | Symbols   String -- ::, ->, etc
  | Function  String
  | Package   String
  | Comment   String
  | Keyword   String -- type, family
  | Newline
  deriving Show

signatureParser :: Parser [HoogleToken]
signatureParser = concat <$> manyTill tokenParser' (char '\n')
  where
    tokenParser' :: Parser [HoogleToken]
    tokenParser' = do
      let seps = ",() -=>[]"
      x <- many $ noneOf ('\n':seps)
      y <- fmap Symbols $ many $ oneOf seps
      let x' = if
            | x == ""          -> Symbols   x
            | x == "family"    -> Keyword   x
            | isUpper $ head x -> Type      x
            | isLower $ head x -> TypeConst x
            | otherwise        -> Symbols   x
      return [x', y]

dataParser :: Parser [HoogleToken]
dataParser = (++[Newline]) <$> mergeL[merge
  [ Package  <$> word
  , Symbols  <$> ws
  , Keyword  <$> string "data"
  ], signatureParser]

functionSignatureParser :: Parser [HoogleToken]
functionSignatureParser = (++[Newline]) <$> mergeL[merge
  [ Package  <$> word
  , Symbols  <$> ws
  , Function <$> word
  , Symbols  <$> ws
  , Symbols  <$> string "::"
  ], signatureParser]

typeAliasParser :: Parser [HoogleToken]
typeAliasParser = merge
  [ Package <$> word
  , Symbols <$> ws
  , Keyword <$> string "type"
  , Symbols <$> ws
  , Type    <$> word
  , Symbols <$> ws
  , Symbols <$> string "="
  , Symbols <$> ws
  , Type    <$> line
  , Newline <$  char '\n'
  ]

typeFamilyParser :: Parser [HoogleToken]
typeFamilyParser = (++[Newline]) <$> mergeL[merge
  [ Package <$> word
  , Symbols <$> ws
  , Keyword <$> string "type family"
  , Symbols <$> ws
  , Type    <$> word
  ], signatureParser]

moduleParser :: Parser [HoogleToken]
moduleParser = do
  kw       <- string "module "
  mod_name <- line
  _        <- char '\n'
  return [Keyword kw, Package mod_name, Newline]

commentParser :: Parser [HoogleToken]
commentParser = do
  _       <- string "--"
  comment <- line
  _       <- char '\n'
  return [Comment "--", Comment comment, Newline]

noResultParser :: Parser [HoogleToken]
noResultParser = do
  x <- string "No results found\n"
  return [Symbols x, Newline]

lineParser :: Parser [HoogleToken]
lineParser = choice $ fmap try
  [ noResultParser
  , commentParser
  , moduleParser
  , typeAliasParser
  , typeFamilyParser
  , dataParser
  , functionSignatureParser
  ]

linesParser :: Parser [HoogleToken]
linesParser = concat <$> manyTill lineParser eof

typeToColored :: HoogleToken -> ColoredString
typeToColored (Type      x) = applyColor brightBlue    x
typeToColored (TypeConst x) = applyColor brightMagenta x
typeToColored (Symbols   x) = applyColor white         x
typeToColored (Comment   x) = applyColor grey          x
typeToColored (Function  x) = applyColor brightGreen   x
typeToColored (Package   x) = applyColor green         x
typeToColored (Keyword   x) = applyColor grey          x
typeToColored  Newline      = applyColor grey          "\n"

runParsers :: String -> Either (ParseErrorBundle String Void) [HoogleToken]
runParsers = parse linesParser "Hoogle output parsing error"

highlightHoogle :: String -> [ColoredString]
highlightHoogle = map typeToColored . fromRight [] . runParsers
