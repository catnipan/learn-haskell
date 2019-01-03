import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph_ a b = Map.Map (a,a) b
type WGraph a = Graph_ a Int

type Graph a = Set.Set (a,a)

type Graph_' a b = Map.Map a [(a,b)]
type Graph' a = Graph_' a Bool
type WGraph' a = Graph_' a Int

graph1 :: Graph Char
graph1 = Set.fromList
  [('S','A'),('S','C'),('S','D'),('D','B'),('C','B'),('A','E'),('A','C'),('E','F'),('E','G'),('G','F'),('G','B')]

breathFirstSearch :: Graph a -> [a]
breathFirstSearch = undefined

depthFirstSearch :: Graph a -> [a]
depthFirstSearch = undefined