{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Set as Set

elem' :: (Eq a) => a -> [a] -> Bool
_ `elem'` [] = False
x `elem'` (x':xs)
  | x == x' = True
  | otherwise = x `elem'` xs

-- each call of elem' takes O(n)
-- in total takes O(n^2)
deduplicate :: forall a. (Eq a) => [a] -> [a]
deduplicate = calcDe []
  where
    calcDe :: [a] -> [a] -> [a]
    calcDe _ [] = []
    calcDe mxs (x:xs)
      | x `elem'` mxs = calcDe mxs xs
      | otherwise = x:(calcDe (x:mxs) xs)

-- drop 20000 $ deduplicate [0..30000]
-- calculation becomes slower and slower

-- if instance Ord a then
-- we can store element we meet in a set
-- thus the lookup takes O(logn)
-- in total it's O(n*logn)
deduplicate' :: forall a. (Ord a) => [a] -> [a]
deduplicate' = calcDe (Set.empty)
  where
    calcDe :: Set.Set a -> [a] -> [a]
    calcDe _ [] = []
    calcDe xset (x:xs)
      | x `Set.member` xset = calcDe xset xs
      | otherwise = x:(calcDe (x `Set.insert` xset) xs)

-- for a sorted xs
-- the lookup only needs to check whether the current element equals the last meet element
-- which takes O(1)
-- the uniquify algorithm takes O(n)
uniquify :: forall a. (Ord a) => [a] -> [a]
uniquify [] = []
uniquify (x:xs) = x:(calcUni x xs)
  where
    calcUni :: a -> [a] -> [a]
    calcUni x [] = []
    calcUni x (x':xs)
      | x == x' = calcUni x xs
      | otherwise = x':(calcUni x' xs)