{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Queue as Queue

type Graph a = Map.Map a [a]
-- implement graph as adjacency list

graph1 :: Graph Char
graph1 = Map.fromList
  [('S',['A','C','D']),
  ('A',['C','E']),
  ('C',['B']),
  ('D',['B']),
  ('E',['G','F']),
  ('G',['B','F'])]

breathFirstSearch :: forall a. (Ord a) => Graph a -> a -> [a]
breathFirstSearch graph start
  | Map.null graph = []
  | otherwise = search Set.empty (Queue.fromList [start])
  where
    firstNode = fst . head . Map.toList $ graph
    search :: Set.Set a -> Queue.Queue a -> [a]
    search seenNodes queues
      | Queue.null queues = []
      | otherwise =
          let (node', queues') = Queue.dequeue queues
              seenNodes' = Set.insert node' seenNodes
              newUnvisitedNodes = filter (not . (`Set.member` seenNodes')) . findNewNodes $ node'
              seenNodes'' = foldr Set.insert seenNodes' newUnvisitedNodes
              newQueues' = foldr Queue.enqueue queues' newUnvisitedNodes
          in node':(search seenNodes'' newQueues')
    findNewNodes :: a -> [a]
    findNewNodes node =
      case Map.lookup node graph of
        Nothing -> []
        Just ns -> ns

depthFirstSearch :: Graph a -> [a]
depthFirstSearch = undefined