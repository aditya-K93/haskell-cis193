{-# OPTIONS_GHC -Wall #-}


module LogAnalysis where

import           Log

parseMessage :: String -> LogMessage
parseMessage message = case words message of
  x : y : z : xs -> case x of
    "E" -> LogMessage (Error $ read y) (read z) (unwords xs)
    "I" -> LogMessage Info (read y) (unwords $ z : xs)
    "W" -> LogMessage Warning (read y) (unwords $ z : xs)
    _   -> Unknown message
  _ -> Unknown message

parse :: String -> [LogMessage]
parse = map parseMessage . lines

singleNodeTree :: LogMessage -> MessageTree
singleNodeTree x = Node Leaf x Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = singleNodeTree msg
insert msg1@(LogMessage _ x _) (Node left msg2@(LogMessage _ n _) right)
  | x < n     = Node (insert msg1 left) msg2 right
  | otherwise = Node left msg2 (insert msg1 right)
insert _ msgTree = msgTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                = []
inOrder (Node left n right) = inOrder left ++ [n] ++ inOrder right

-- inOrder  <$> build <$> testParse parse 100 "error.log"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = searchErrorString . inOrder . build . filter (severity 50)

severity :: Int -> LogMessage -> Bool
severity s (LogMessage (Error n) _ _) | n > s     = True
                                      | otherwise = False
severity _ _ = False

searchErrorString :: [LogMessage] -> [String]
searchErrorString = map (\(LogMessage _ _ x) -> x)

















