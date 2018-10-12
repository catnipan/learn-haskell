-- Given a string, find the length of the longest substring without repeating characters.

-- Example 1:

-- Input: "abcabcbb"
-- Output: 3 
-- Explanation: The answer is "abc", with the length of 3. 
-- Example 2:

-- Input: "bbbbb"
-- Output: 1
-- Explanation: The answer is "b", with the length of 1.
-- Example 3:

-- Input: "pwwkew"
-- Output: 3
-- Explanation: The answer is "wke", with the length of 3. 
--              Note that the answer must be a substring, "pwke" is a subsequence and not a substring.

import Data.List (inits, sort, group)
import qualified Data.Set as Set

type Length = Int

lengthOfLongestSubstring :: String -> Length
lengthOfLongestSubstring "" = 0
lengthOfLongestSubstring str = max (maxLenAtStart str Set.empty 0) (lengthOfLongestSubstring $ tail str)
  where
    maxLenAtStart :: String -> Set.Set Char -> Length -> Length
    maxLenAtStart [] _ currLen = currLen
    maxLenAtStart (s:ss) charSet currLen =
      if s `Set.member` charSet
        then currLen
        else maxLenAtStart ss (Set.insert s charSet) (currLen + 1)