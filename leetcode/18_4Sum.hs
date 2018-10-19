-- Given an array nums of n integers and an integer target, are there elements a, b, c, and d in nums such that a + b + c + d = target? Find all unique quadruplets in the array which gives the sum of target.

-- Note:

-- The solution set must not contain duplicate quadruplets.

-- Example:

-- Given array nums = [1, 0, -1, 0, -2, 2], and target = 0.

-- A solution set is:
-- [
--   [-1,  0, 0, 1],
--   [-2, -1, 1, 2],
--   [-2,  0, 0, 2]
-- ]

import Data.List(sort,nubBy)

toQuadrupletsWith :: (a -> a -> a -> a -> [b]) -> [a] -> [b]
toQuadrupletsWith f xs = do
  (idxA,a) <- idxXs
  (idxB,b) <- drop idxA idxXs
  (idxC,c) <- drop idxB idxXs
  (_,d) <- drop idxC idxXs
  f a b c d
  where idxXs = zip [1..] xs

fourSum :: [Int] -> Int -> [(Int,Int,Int,Int)]
fourSum xs target = nubBy isSameQuadruplets . toQuadrupletsWith isSolutionMaybe $ xs
  where
    isSolutionMaybe :: Int -> Int -> Int -> Int -> [(Int,Int,Int,Int)]
    isSolutionMaybe a b c d = if a + b + c + d == target then [(a,b,c,d)] else []
    isSameQuadruplets :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Bool
    isSameQuadruplets (a,b,c,d) (e,f,g,j) = sort [a,b,c,d] == sort [e,f,g,j]