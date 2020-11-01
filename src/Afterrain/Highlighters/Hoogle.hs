{-# LANGUAGE MultiWayIf #-}
module Afterrain.Highlighters.Hoogle where

import           Data.Char              (isLower, isUpper)
import           Data.Void              (Void)
import           Rainbow
import           Text.Megaparsec        hiding (tokens)
import           Text.Megaparsec.Char

import           MyLogger

import           Afterrain.Utils.Colors
import           Afterrain.Utils.Parser
import Afterrain.Utils.Loggers

data HoogleToken =
    Type      String
  | TypeConst String
  | Symbols   String -- ::, ->, etc
  | Function  String
  | Package   String
  | Comment   String
  | Keyword   String -- type, family
  | Query     String
  | Newline
  deriving Show

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

getColor :: (HoogleConfig -> Color) -> (HoogleConfig -> Color) -> HoogleConfig -> Color
getColor c1 c2 conf = c1 conf <> only256 (c2 conf)

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
            | x == "forall"    -> Keyword   x
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
typeAliasParser = (++[Newline]) <$> mergeL [merge
  [ Package   <$> word
  , Symbols   <$> ws
  , Keyword   <$> string "type"
  , Symbols   <$> ws
  , Type      <$> word
  , Symbols   <$> ws
  , TypeConst <$> many (anySingleBut '=')
  , Symbols   <$> string "="
  ], signatureParser]

typeFamilyParser :: Parser [HoogleToken]
typeFamilyParser = (++[Newline]) <$> mergeL[merge
  [ Package <$> word
  , Symbols <$> ws
  , Keyword <$> string "type family"
  , Symbols <$> ws
  , Type    <$> word
  ], signatureParser]

packageParser :: Parser [HoogleToken]
packageParser = merge
  [ Keyword <$> string "package "
  , Package <$> line
  , Newline <$  char '\n'
  ]

moduleParser :: Parser [HoogleToken]
moduleParser = merge
  [ Keyword <$> string "module "
  , Package <$> line
  , Newline <$  char '\n'
  ]

commentParser :: Parser [HoogleToken]
commentParser = merge
  [ Comment <$> string "--"
  , Comment <$> line
  , Newline <$  char '\n'
  ]

noResultParser :: Parser [HoogleToken]
noResultParser = pure . Symbols <$> string "No results found\n"

verboseQueryParser :: Parser [HoogleToken]
verboseQueryParser = merge
  [ Keyword <$> string "Query: "
  , Query   <$> line
  , Newline <$  newline
  ]

lineParser :: Parser [HoogleToken]
lineParser = choice $ fmap try
  [ noResultParser
  , commentParser
  , verboseQueryParser
  , moduleParser
  , packageParser
  , typeAliasParser
  , typeFamilyParser
  , dataParser
  , functionSignatureParser
  ]

linesParser :: Parser [HoogleToken]
linesParser = concat <$> manyTill lineParser eof

typeToColored :: HoogleToken -> HoogleConfig  -> ColoredString
typeToColored (Type      x) c = applyColor x    $ getColor typeColor8      typeColor256      c
typeToColored (TypeConst x) c = applyColor x    $ getColor typeConstColor8 typeConstColor256 c
typeToColored (Symbols   x) c = applyColor x    $ getColor symbolsColor8   symbolsColor256   c
typeToColored (Comment   x) c = applyColor x    $ getColor commentColor8   commentColor256   c
typeToColored (Function  x) c = applyColor x    $ getColor functionColor8  functionColor256  c
typeToColored (Package   x) c = applyColor x    $ getColor packageColor8   packageColor256   c
typeToColored (Keyword   x) c = applyColor x    $ getColor keywordColor8   keywordColor256   c
typeToColored (Query     x) c = applyColor x    $ getColor queryColor8     queryColor256     c
typeToColored  Newline      c = applyColor "\n" $ getColor newlineColor8   newlineColor256   c

runParsers :: String -> Either (ParseErrorBundle String Void) [HoogleToken]
runParsers = parse linesParser "Hoogle output parsing error"

highlightHoogle :: HoogleConfig -> String -> Logger [ColoredString]
highlightHoogle conf inp = case tokens of
  Left a        -> failWithLogs [Log Error ("Failed decoding input: " ++ show a)]
  Right tokens' -> returnWithLogs [Log Debug "Highlighted input"] $ map (`typeToColored` conf) tokens'
  where
    tokens = runParsers inp
