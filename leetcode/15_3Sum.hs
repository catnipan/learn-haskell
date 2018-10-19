-- Given an array nums of n integers, are there elements a, b, c in nums such that a + b + c = 0? Find all unique triplets in the array which gives the sum of zero.

-- Note:

-- The solution set must not contain duplicate triplets.

-- Example:

-- Given array nums = [-1, 0, 1, 2, -1, -4],

-- A solution set is:
-- [
--   [-1, 0, 1],
--   [-1, -1, 2]
-- ]

import Data.List(sort,nubBy)

type SolutionTriple = (Int, Int, Int)

getAllTriples :: [Int] -> [SolutionTriple]
getAllTriples nums = do
  (aIdx,a) <- idxNums
  (bIdx,b) <- drop aIdx idxNums
  (_,c) <- drop bIdx idxNums
  return (a,b,c)
  where idxNums = zip [1..] nums

isDuplicateTriplets :: SolutionTriple -> SolutionTriple -> Bool
isDuplicateTriplets (a,b,c) (d,e,f) = sort [a,b,c] == sort [d,e,f]

threeSum :: [Int] -> [SolutionTriple]
threeSum nums = nubBy isDuplicateTriplets . filter sumEqual0 . getAllTriples $ nums
  where sumEqual0 (x,y,z) = x + y + z == 0