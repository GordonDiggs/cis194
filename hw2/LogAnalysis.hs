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

messageTimestamp :: LogMessage -> TimeStamp
messageTimestamp (LogMessage _ timestamp _) = timestamp
messageTimestamp (Unknown _) = undefined

treeMessage :: MessageTree -> LogMessage
treeMessage (Node _ msg _) = msg
treeMessage (Leaf) = undefined

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg tree
  | messageTimestamp msg <= messageTimestamp (treeMessage tree) = Node Leaf msg tree
  | messageTimestamp msg > messageTimestamp (treeMessage tree) = Node tree msg Leaf
