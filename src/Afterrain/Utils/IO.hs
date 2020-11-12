{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Afterrain.Utils.IO where

import           RIO
import qualified System.IO     as IO
import System.Environment as Env

print :: Show a => a -> RIO b ()
print = liftIO . IO.print

getContents :: RIO a String
getContents = liftIO IO.getContents

getEnv :: String -> RIO a String
getEnv = liftIO . fmap (fromMaybe "") . Env.lookupEnv

mkLog :: (Semigroup a, IsString a) => a -> a -> a
mkLog a b = a <> " (" <> b <> ")"

mkLog' :: Show a => Utf8Builder -> a -> Utf8Builder
mkLog' a b = a <> " (" <> displayShow b <> ")"

