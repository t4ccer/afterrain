{-# LANGUAGE MultiWayIf #-}
module Afterrain.Highlighters.Hoogle (printHoogle) where

import           Data.Char                (isLower, isUpper)
import           Data.Void                (Void)
import           Rainbow
import           Text.Megaparsec          hiding (tokens)
import           Text.Megaparsec.Char

import           MyIOLogger
import           MyLogger

import           Afterrain.Configs
import           Afterrain.Configs.Hoogle
import           Afterrain.Utils.Colors
import           Afterrain.Utils.Loggers
import           Afterrain.Utils.Parser

data HoogleToken =
    Type      String
  | TypeConst String
  | Symbols   String -- ::, ->, etc
  | Function  String
  | Package   String
  | Comment   String
  | Keyword   String -- type, family
  | Query     String
  | Unknown   String
  | Newline
  deriving Show

getColor :: (a -> Color) -> (a -> Color) -> a -> Color
getColor c1 c2 conf = Color256 $ unColor (c1 conf) <> only256 (unColor $ c2 conf)

addNewLine :: Parser [HoogleToken]
addNewLine = pure $ return Newline

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
dataParser = mergeL
  [ merge
    [ Package  <$> word
    , Symbols  <$> ws
    , Keyword  <$> string "data"
    ]
  , signatureParser
  , addNewLine
  ]

functionSignatureParser :: Parser [HoogleToken]
functionSignatureParser = mergeL
  [ merge
    [ Package  <$> word
    , Symbols  <$> ws
    , Function <$> word
    , Symbols  <$> ws
    , Symbols  <$> string "::"
    ]
  , signatureParser
  , addNewLine
  ]

typeAliasParser :: Parser [HoogleToken]
typeAliasParser = mergeL
  [ merge
    [ Package   <$> word
    , Symbols   <$> ws
    , Keyword   <$> string "type"
    , Symbols   <$> ws
    , Type      <$> word
    , Symbols   <$> ws
    , TypeConst <$> many (anySingleBut '=')
    , Symbols   <$> string "="
    ]
  , signatureParser
  , addNewLine
  ]

typeFamilyParser :: Parser [HoogleToken]
typeFamilyParser = mergeL
  [ merge
    [ Package <$> word
    , Symbols <$> ws
    , Keyword <$> string "type family"
    , Symbols <$> ws
    , Type    <$> word
    ]
  , signatureParser
  , addNewLine
  ]

packageParser :: Parser [HoogleToken]
packageParser = merge
  [ Keyword <$> string "package "
  , Package <$> line
  , Newline <$  char '\n'
  ]

classParser :: Parser [HoogleToken]
classParser = mergeL
  [ merge
    [ Package <$> word
    , Symbols <$> ws
    , Keyword <$> string "class"
    ] 
  , signatureParser
  , addNewLine
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

unknownParser :: Parser [HoogleToken]
unknownParser = merge
  [ Unknown <$> line
  , Newline <$  newline
  ]

linesParser :: Parser [HoogleToken]
linesParser = concat <$> manyTill lineParser eof

lineParser :: Parser [HoogleToken]
lineParser = choice $ fmap try
  [ noResultParser
  , commentParser
  , verboseQueryParser
  , moduleParser
  , packageParser
  , typeAliasParser
  , typeFamilyParser
  , classParser
  , dataParser
  , functionSignatureParser
  , unknownParser
  ]

typeToColored :: HoogleToken -> HoogleConfig  -> ColoredString
typeToColored (Type      x) c = applyColor x    $ getColor typeColor8      typeColor256      c
typeToColored (TypeConst x) c = applyColor x    $ getColor typeConstColor8 typeConstColor256 c
typeToColored (Symbols   x) c = applyColor x    $ getColor symbolsColor8   symbolsColor256   c
typeToColored (Comment   x) c = applyColor x    $ getColor commentColor8   commentColor256   c
typeToColored (Function  x) c = applyColor x    $ getColor functionColor8  functionColor256  c
typeToColored (Package   x) c = applyColor x    $ getColor packageColor8   packageColor256   c
typeToColored (Keyword   x) c = applyColor x    $ getColor keywordColor8   keywordColor256   c
typeToColored (Query     x) c = applyColor x    $ getColor queryColor8     queryColor256     c
typeToColored (Unknown   x) c = applyColor x    $ getColor unknownColor8   unknownColor256   c
typeToColored  Newline      c = applyColor "\n" $ getColor (Color8 . const white) (Color256 . const white)   c

runParsers :: String -> Either (ParseErrorBundle String Void) [HoogleToken]
runParsers = parse linesParser "Hoogle output parsing error"

highlightHoogle :: HoogleConfig -> String -> Logger [ColoredString]
highlightHoogle conf inp = case tokens of
  Left a        -> failWithLogs [Log Error ("Failed decoding input: " ++ show a)]
  Right tokens' -> returnWithLogs [Log Debug "Highlighted input"] $ map (`typeToColored` conf) tokens'
  where
    tokens = runParsers inp

printHoogle :: String -> IOLogger ()
printHoogle input = do
  config <- hoogleConfig <$> readConfigFile
  strs <- liftLogger $ highlightHoogle config input
  appendIOLogs ignore (debugLog "Printed highlighted strings") $ fromIO $ printColoredStrings strs
