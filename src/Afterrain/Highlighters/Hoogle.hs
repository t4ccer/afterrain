{-# LANGUAGE MultiWayIf #-}
module Afterrain.Highlighters.Hoogle (highlightHoogle)  where

import           Control.Monad
import           Data.Char
import           Data.Either            (fromRight)
import           Data.Void
import           Text.Megaparsec        hiding (chunk)
import           Text.Megaparsec.Char

import           Afterrain.Utils.Colors
import           Afterrain.Utils.Parser

data HoogleToken =
    Type      String
  | TypeConst String
  | Symbols   String
  | Function  String
  | Package   String
  | Comment   String
  | Keyword   String -- type, family
  deriving Show

functionNameParser :: Parser [HoogleToken]
functionNameParser = do
  package <- many $ anySingleBut ' '
  s1      <- some $ char ' '
  fName   <- many $ anySingleBut ' '
  s2      <- some $ char ' '
  x <- concat <$> manyTill tokenParser' (char '\n')

  let f = if
          | fName == "type" -> Keyword fName
          | isUpper $ head fName -> Type fName
          | otherwise -> Function fName

  return $ [Package package, Symbols s1, f, Symbols s2] <> x <> [Symbols "\n"]

tokenParser' :: Parser [HoogleToken]
tokenParser' = do
  let seps = ",() -=>"
  x <- many $ noneOf ('\n':seps)
  y <- fmap Symbols $ many $ oneOf  seps
  let x' = if
        | x == "family"    -> Keyword   x
        | isUpper $ head x -> Type      x
        | isLower $ head x -> TypeConst x
        | otherwise        -> Symbols   x
  return [x', y]

commentParser' :: Parser [HoogleToken]
commentParser' = do
  _ <- string "--"
  x <- manyTill anySingle (void (oneOf "\n") <|> eof)
  return [Comment "--", Comment x, Comment "\n"]

noResultParser :: Parser [HoogleToken]
noResultParser = do
  _ <- string "No results found"
  return [Symbols "No results found\n"]

resultParser :: Parser [HoogleToken]
resultParser = concat <$> someTill (choice $ fmap try
  [ commentParser'
  , functionNameParser
  ]) eof

hoogleParser' :: Parser [HoogleToken]
hoogleParser' = choice $ fmap try [noResultParser, resultParser]

typeToColored :: HoogleToken -> ColoredString
typeToColored (Type      x) = applyColor brightBlue    x
typeToColored (TypeConst x) = applyColor brightMagenta x
typeToColored (Symbols   x) = applyColor white         x
typeToColored (Comment   x) = applyColor grey          x
typeToColored (Function  x) = applyColor brightGreen   x
typeToColored (Package   x) = applyColor green         x
typeToColored (Keyword   x) = applyColor grey          x

runParsers :: String -> Either (ParseErrorBundle String Void) [HoogleToken]
runParsers = parse hoogleParser' "Hoogle output parsing error"

highlightHoogle :: String -> [ColoredString]
highlightHoogle = map typeToColored . fromRight [] . runParsers
