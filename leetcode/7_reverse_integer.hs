-- Given a 32-bit signed integer, reverse digits of an integer.

-- Example 1:

-- Input: 123
-- Output: 321
-- Example 2:

-- Input: -123
-- Output: -321
-- Example 3:

-- Input: 120
-- Output: 21
-- Note:
-- Assume we are dealing with an environment which could only store integers within the 32-bit signed integer range: [−231,  231 − 1]. For the purpose of this problem, assume that your function returns 0 when the reversed integer overflows.

import Data.Int(Int32)
import Prelude hiding(reverse)

reverse :: Int32 -> Int32
reverse int = mayNegate . intToReverseInt 0 . mayNegate $ int
  where
    mayNegate = if int < 0 then negate else id
    intToReverseInt :: Int32 -> Int32 -> Int32
    intToReverseInt resInt int =
      if int == 0
        then resInt
        else intToReverseInt newResInt (int `quot` 10)
      where newResInt = (\v -> if v < 0 then 0 else v) $ resInt * 10 + (int `mod` 10)
