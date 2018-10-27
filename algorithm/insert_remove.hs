type Rank = Int

-- O(n)
insert :: Rank -> a -> [a] -> [a]
insert 0 x' xs = x':xs
insert i x' (x:xs) = x:(insert (i-1) x' xs)
insert i x' [] = error $ "rank " ++ show i ++ " exceeds length"

-- O(n)
remove :: Rank -> Rank -> [a] -> [a]
remove 0 hi (x:xs)
  | hi < 0 = error $ "rank " ++ show lo ++ " is larger than " ++ show hi
  | hi == 0 = xs
  | otherwise = remove 0 (hi-1) xs
remove 0 hi [] = error $ "rank " ++ show hi ++ " exceeds length"
remove lo hi (x:xs) = x:(remove (lo-1) (hi-1) xs)
remove lo _ [] = error $ "rank " ++ show lo ++ " exceeds length"