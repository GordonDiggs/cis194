{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case words(str) of
  ("I":time:msg) -> LogMessage Info (read time :: Int) (unwords(msg))
  ("E":code:time:msg) -> LogMessage (Error (read code :: Int)) (read time :: Int) (unwords(msg))
  ("W":time:msg) -> LogMessage Warning (read time :: Int) (unwords(msg))
  _ -> Unknown "This is not in the right format"

parse :: String -> [LogMessage]
parse str = map parseMessage (lines(str))
