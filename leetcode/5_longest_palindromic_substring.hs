-- Given a string s, find the longest palindromic substring in s. You may assume that the maximum length of s is 1000.

-- Example 1:

-- Input: "babad"
-- Output: "bab"
-- Note: "aba" is also a valid answer.
-- Example 2:

-- Input: "cbbd"
-- Output: "bb"

import Data.List (inits)

type Length = Int

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

longestPalindrome :: String -> String
longestPalindrome [s] = [s]
longestPalindrome string =
  if length maxStrStartAtS > length maxRestStr then maxStrStartAtS else maxRestStr
  where
    maxStrStartAtS = longestPalindromStartAtFirst string
    maxRestStr = longestPalindrome . tail $ string
    longestPalindromStartAtFirst str = foldl1 getMaxPalindromStr $ inits str
      where
        getMaxPalindromStr maxStr currStr =
          if isPalindrome currStr && length currStr > length maxStr
            then currStr
            else maxStr