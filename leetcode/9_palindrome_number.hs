-- Determine whether an integer is a palindrome. An integer is a palindrome when it reads the same backward as forward.

-- Example 1:

-- Input: 121
-- Output: true
-- Example 2:

-- Input: -121
-- Output: false
-- Explanation: From left to right, it reads -121. From right to left, it becomes 121-. Therefore it is not a palindrome.
-- Example 3:

-- Input: 10
-- Output: false
-- Explanation: Reads 01 from right to left. Therefore it is not a palindrome.

-- Follow up:

-- Coud you solve it without converting the integer to a string?

isPalindrome :: Int -> Bool
isPalindrome = isPldStr . show
  where isPldStr str = reverse str == str

isPalindrome' :: Int -> Bool
isPalindrome' int = if int < 0 then False else calcIsPld int digitsCnt
  where
    digitsCnt = (+1) . floor . logBase 10 . fromIntegral $ int
    calcIsPld :: Int -> Int -> Bool
    calcIsPld int digitsCnt =
      if digitsCnt < 2
        then True
        else if highestDigit /= lowesetDigit
              then False
              else calcIsPld ((int `mod` highestPower) `quot` 10) (digitsCnt - 2)
      where highestPower = 10 ^ (digitsCnt - 1)
            highestDigit = int `quot` highestPower
            lowesetDigit = int `mod` 10