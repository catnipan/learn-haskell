-- Given a collection of candidate numbers (candidates) and a target number (target), find all unique combinations in candidates where the candidate numbers sums to target.

-- Each number in candidates may only be used once in the combination.

-- Note:

-- All numbers (including target) will be positive integers.
-- The solution set must not contain duplicate combinations.
-- Example 1:

-- Input: candidates = [10,1,2,7,6,1,5], target = 8,
-- A solution set is:
-- [
--   [1, 7],
--   [1, 2, 5],
--   [2, 6],
--   [1, 1, 6]
-- ]
-- Example 2:

-- Input: candidates = [2,5,2,1,2], target = 5,
-- A solution set is:
-- [
--   [1,2,2],
--   [5]
-- ]

import Data.List(sort, group)

type Count = Int

combinationSum :: [Int] -> Int -> [[Int]]
combinationSum xs = getcsum intCnts
  where
    getcsum :: [(Int, Count)] -> Int -> [[Int]]
    getcsum [] 0 = [[]]
    getcsum [] _ = []
    getcsum ((x,xcnt):xs) target = [0..xcnt] >>= getSum
      where
        getSum :: Count -> [[Int]]
        getSum cnt = map ((replicate cnt x) ++) $ getcsum xs (target - cnt * x)
    intCnts :: [(Int, Count)]
    intCnts = map getIntCnt . group . sort $ xs
      where
        getIntCnt :: [Int] -> (Int, Count)
        getIntCnt xs@(x:_) = (x, length xs)