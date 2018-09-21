import qualified Data.Map as Map

-- 关联列表 [(a,b)]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k,v):xs) = if key == k then Just v else (findKey key xs)
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- Map.fromList :: Ord k => [(k, a)] -> Map.Map k a
-- Map.toList :: Map.Map k a -> [(k, a)]
-- Map.empty :: Map.Map k a

-- Map.fromList [("betty", "555-2938"),("bonnie", "452-2928"),("lucille","500-2134")]
-- Map.insert 3 100 Map.empty
-- fromList [(3,100)]

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- Map.null :: Map.Map k a -> Bool
-- Map.size :: Map.Map k a -> Int
-- 判断是否为空、大小

-- Map.singleton k a = Map.insert k a Map.empty

-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
-- Map.member :: Ord k => k -> Map.Map k a -> Bool
-- Map.map :: (a -> b) -> Map.Map k a -> Map.Map k b
-- Map.filter :: (a -> Bool) -> Map.Map k a -> Map.Map k a

-- Map.keys :: Map.Map k a -> [k]
-- keys' = Map.map fst . Map.toList
-- Map.elems :: Map.Map k a -> [a]
-- elems' = Map.map snd . Map.toList

-- Map.fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map.Map k a


phoneBook = [("betty", "111111"),
  ("betty", "222222"),
  ("bonnie", "333333")]

phoneBookToMap :: [(String, String)] -> Map.Map String [String]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs