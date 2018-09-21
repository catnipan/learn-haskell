import qualified Data.Map as Map
-- 生成 path 的通用 treeToMap

data ValueNode k a = Node (k, a, Tree k a) deriving (Show)
data Tree k a = NodeList [(ValueNode k a)] deriving (Show)

data Path k = Path [k]
data DataMapValue k a = DataMapValue (a, Path k)

treeToMap :: (Ord k) => Tree k a -> Map.Map k (DataMapValue k a)
treeToMap nodeList = calcTreeToMap nodeList Map.empty (Path [])
  where 
    calcTreeToMap :: (Ord k) => Tree k a -> Map.Map k (DataMapValue k a) -> Path k -> Map.Map k (DataMapValue k a)
    calcTreeToMap (NodeList ((Node (key, value, childrenTree):restNodes))) treeMap path =
      Map.insert key (DataMapValue (value, newPath)) $ Map.union childrenNodesMap restNodesMap
      where
        newPath = let (Path oldPath) = path in Path (oldPath ++ [key])
        childrenNodesMap = calcTreeToMap childrenTree treeMap newPath
        restNodesMap = calcTreeToMap (NodeList restNodes) treeMap path
    calcTreeToMap _ treeMap _ = treeMap

mapTree :: (Ord k) => (a -> b) -> Tree k a -> Tree k b
mapTree func (NodeList (Node (key, value, childrenTree):restNodes)) =
  NodeList (Node (key, func value, newChildrenTree):newRestNodes)
  where newChildrenTree = mapTree func childrenTree
        (NodeList newRestNodes) = mapTree func (NodeList restNodes)