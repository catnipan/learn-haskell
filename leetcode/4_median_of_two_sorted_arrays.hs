-- There are two sorted arrays nums1 and nums2 of size m and n respectively.

-- Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).

-- You may assume nums1 and nums2 cannot be both empty.

-- Example 1:

-- nums1 = [1, 3]
-- nums2 = [2]

-- The median is 2.0
-- Example 2:

-- nums1 = [1, 2]
-- nums2 = [3, 4]

-- The median is (2 + 3)/2 = 2.5

{-# LANGUAGE ScopedTypeVariables #-}

import Data.Function(on)
import Data.List(genericLength)

average :: Fractional a => [a] -> a
average xs = (sum xs) / (genericLength xs)

findMedianSortedArrays :: forall a.(Fractional a, Ord a) => [a] -> [a] -> a
findMedianSortedArrays xs ys = average . getMediums $ merge xs ys
  where
    getMediums = (if even totalLen then drop (halfLen - 1) else drop halfLen) . take (halfLen + 1)
    halfLen = totalLen `quot` 2
    totalLen = ((+) `on` length) xs ys
    merge :: [a] -> [a] -> [a]
    merge xxs@(x:xs) yys@(y:ys) = if x < y then x:(merge xs yys) else y:(merge xxs ys)
    merge xs [] = xs
    merge [] ys = ys
