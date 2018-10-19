module MyUtils.LinkedList(
  LinkedList,
  fromList,
  toList,
) where

data LinkedList a =
  Node { getCurrVal :: a , getNextNode :: LinkedList a } 
  | EndNode

instance (Show a) => Show (LinkedList a) where
  show (Node curr nxNode) = show curr ++ "->" ++ show nxNode
  show EndNode = "∅"

fromList :: [a] -> LinkedList a
fromList [] = EndNode
fromList (x:xs) = Node x (fromList xs)

toList :: LinkedList a -> [a]
toList EndNode = []
toList (Node x nxNode) = x:(toList nxNode)