-- You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.

-- You may assume the two numbers do not contain any leading zero, except the number 0 itself.

-- Example:

-- Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
-- Output: 7 -> 0 -> 8
-- Explanation: 342 + 465 = 807.

addTwoNumbers :: [Int] -> [Int] -> [Int]
addTwoNumbers = add 0
  where
    add carry (x:xs) (y:ys) =
      let sum = x + y + carry
          (nint, ncarry) = (sum `mod` 10, sum `quot` 10)
      in nint:(add ncarry xs ys)
    add carry xs@(x:_) [] = add carry xs [0]
    add carry [] ys@(y:_) = add carry [0] ys
    add carry [] []
      | carry == 0 = []
      | otherwise = [carry]