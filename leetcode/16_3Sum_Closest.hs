-- Given an array nums of n integers and an integer target, find three integers in nums such that the sum is closest to target. Return the sum of the three integers. You may assume that each input would have exactly one solution.

-- Example:

-- Given array nums = [-1, 2, 1, -4], and target = 1.

-- The sum that is closest to the target is 2. (-1 + 2 + 1 = 2).

import Data.Function(on)
import Data.List(minimumBy)
import MyUtil(toUniqTriple)

threeSumClosest :: [Int] -> Int -> Int
threeSumClosest nums target = minimumBy distanceToTarget . map tripleSum . toUniqTriple $ nums
  where
    distanceToTarget = compare `on` (abs . (subtract target))
    tripleSum (a,b,c) = a + b + c