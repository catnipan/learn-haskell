import qualified Data.Map as Map
-- 生成 path 的通用 treeToMap

data Node k a = Node (k, a, Tree k a) deriving (Show)
data Tree k a = Tree [(Node k a)] deriving (Show)
data Path k = Path [k]

treeToMap :: (Ord k) => Tree k a -> Map.Map k (a, Path k)
treeToMap tree = calcTreeToMap tree Map.empty (Path [])
  where 
    calcTreeToMap :: (Ord k) => Tree k a -> Map.Map k (a, Path k) -> Path k -> Map.Map k (a, Path k)
    calcTreeToMap (Tree (Node (key, value, childrenTree):restNodes)) treeMap path =
      Map.insert key (value, newPath) $ Map.union childrenNodesMap restNodesMap
      where
        newPath = let (Path oldPath) = path in Path (oldPath ++ [key])
        childrenNodesMap = calcTreeToMap childrenTree treeMap newPath
        restNodesMap = calcTreeToMap (Tree restNodes) treeMap path
    calcTreeToMap _ treeMap _ = treeMap

mapTree :: (Ord k) => (a -> b) -> Tree k a -> Tree k b
mapTree func (Tree (Node (key, value, childrenTree):restNodes)) =
  Tree (Node (key, func value, newChildrenTree):newRestNodes)
  where newChildrenTree = mapTree func childrenTree
        (Tree newRestNodes) = mapTree func (Tree restNodes)