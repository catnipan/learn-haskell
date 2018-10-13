-- You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.

-- You may assume the two numbers do not contain any leading zero, except the number 0 itself.

-- Example:

-- Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
-- Output: 7 -> 0 -> 8
-- Explanation: 342 + 465 = 807.

type DigitList = [Int]

zipDigitList :: DigitList -> DigitList -> [(Int, Int)]
zipDigitList (x:xs) (y:ys) = (x,y):(zipDigitList xs ys)
zipDigitList [] [] = []
zipDigitList [] yys = zipDigitList [0] yys
zipDigitList xxs [] = zipDigitList xxs [0]

addTwoNumbers :: DigitList -> DigitList -> DigitList
addTwoNumbers nl ml = getResult . foldl foldFunction ([], 0) $ zipDigitList nl ml
  where
    foldFunction (rs, c) (n,m) =
      let sum = c + n + m
          nr = sum `mod` 10
          nc = sum `quot` 10
      in ((nr:rs), nc)
    getResult (rs, c) = reverse $ if c == 0 then rs else c:rs