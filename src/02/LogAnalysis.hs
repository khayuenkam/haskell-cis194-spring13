{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage msg = let wordList = words msg in
                    case wordList of
                        ("I" : timestamp: xs) -> LogMessage Info (read timestamp) (unwords xs)
                        ("W" : timestamp: xs) -> LogMessage Warning (read timestamp) (unwords xs)
                        ("E" : level : timestamp: xs) -> LogMessage (Error $ read level) (read timestamp) (unwords xs)
                        message -> Unknown (unwords message)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
    | ts1 > ts2 = Node left msg2 (insert msg1 right)
    | otherwise = Node (insert msg1 left) msg2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ msg) -> msg) . inOrder . build . filter isSevere
    where isSevere :: LogMessage -> Bool
          isSevere (LogMessage (Error level) _ _) = level >= 50
          isSevere _ = False