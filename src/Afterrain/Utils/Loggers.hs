module Afterrain.Utils.Loggers where

import MyLogger
import MyIOLogger

data LogLevel = 
    Debug
  | Error
  deriving (Show, Eq)

data Log = Log LogLevel String
  deriving Show

type Logger a = MyLogger [Log] a
type IOLogger a = MyIOLogger [Log] a

ignore :: [Log] -> IO ()
ignore = return . const ()

printLog :: Log -> IO ()
printLog (Log level msg) = putStrLn ("["++show level++"] "++msg)

printLogs :: [Log] -> IO ()
printLogs = mapM_ printLog

printErrorLogs :: [Log] -> IO ()
printErrorLogs = printLogs . filter (\(Log level _) -> level == Error)

runIOLogger :: Monoid w => MyIOLogger w a -> IO (Maybe a, w)
runIOLogger = runMyIOLogger

runLogger :: Monoid w => MyLogger w a -> (Maybe a, w)
runLogger = runMyLogger

errorLog :: String -> [Log]
errorLog = pure . Log Error

debugLog :: String -> [Log]
debugLog = pure . Log Debug

fromIOWithDebugLog :: ([Log] -> IO ()) -> String -> IO a -> MyIOLogger [Log] a
fromIOWithDebugLog f m = appendIOLogs f (debugLog m) . fromIO

failWithoutLogs :: IOLogger a
failWithoutLogs = failWithIOLogs ignore []