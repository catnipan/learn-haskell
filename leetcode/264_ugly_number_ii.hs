-- Write a program to find the n-th ugly number.

-- Ugly numbers are positive numbers whose prime factors only include 2, 3, 5. 

-- Example:

-- Input: n = 10
-- Output: 12
-- Explanation: 1, 2, 3, 4, 5, 6, 8, 9, 10, 12 is the sequence of the first 10 ugly numbers.

-- Note:  

--     1 is typically treated as an ugly number.
--     n does not exceed 1690.

import Data.Function(on)

uglyNumbers :: [Int]
uglyNumbers = iterate [1]
  where
    iterate :: [Int] -> [Int]
    iterate (s:xs) = s:(iterate (merge [s*2,s*3,s*5] xs))
      where
        merge :: [Int] -> [Int] -> [Int]
        merge xss@(x:xs) yss@(y:ys)
          | x < y = x:(merge xs yss)
          | y < x = y:(merge xss ys)
          | x == y = x:(merge xs ys)
        merge [] ys = ys
        merge xs [] = xs

nthUglyNumber :: Int -> Int
nthUglyNumber n = head . drop (n-1) $ uglyNumbers
