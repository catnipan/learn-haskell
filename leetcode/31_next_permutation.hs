-- Implement next permutation, which rearranges numbers into the lexicographically next greater permutation of numbers.

-- If such arrangement is not possible, it must rearrange it as the lowest possible order (ie, sorted in ascending order).

-- The replacement must be in-place and use only constant extra memory.

-- Here are some examples. Inputs are in the left-hand column and its corresponding outputs are in the right-hand column.

-- 1,2,3 → 1,3,2
-- 3,2,1 → 1,2,3
-- 1,1,5 → 1,5,1

import Data.List(sort)

data Permus = Pending [Int] Int | Solved [Int]

nextPermutation :: [Int] -> [Int]
nextPermutation ds = case res of
                      (Pending _ i) -> [1..i]
                      (Solved xs) -> xs
  where
    res = foldr foldFun (Pending [] 0) ds
    foldFun :: Int -> Permus -> Permus
    foldFun d (Pending [] 0) = (Pending [d] d)
    foldFun d (Pending ds maxD) =
      if (d > maxD)
        then Pending (d:ds) d
        else let (f:s:rest) = sort (d:ds)
             in Solved (s:f:rest)
    foldFun d (Solved ds) = Solved (d:ds)