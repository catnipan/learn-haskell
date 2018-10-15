-- You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.

-- You may assume the two numbers do not contain any leading zero, except the number 0 itself.

-- Example:

-- Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
-- Output: 7 -> 0 -> 8
-- Explanation: 342 + 465 = 807.

import Data.Function(on)

type DigitList = [Int]

zipDigitList :: DigitList -> DigitList -> [(Int, Int)]
zipDigitList xs ys = take ((max `on` length) xs ys) $ zip (xs++repeat 0) (ys++repeat 0)

addTwoNumbers :: DigitList -> DigitList -> DigitList
addTwoNumbers nl ml = getResult . foldl foldFunction [0] $ zipDigitList nl ml
  where
    foldFunction (c:res) (n,m) =
      let sum = c + n + m
          nr = sum `mod` 10
          nc = sum `quot` 10
      in (nc:nr:res)
    getResult res = reverse $ if head res == 0 then tail res else res