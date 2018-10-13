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

import Data.List (break)
import qualified Data.Set as Set

-- use list as a FIFO queue
type StringQueue = String
type Length = Int

lengthOfLongestSubstring :: String -> Length
lengthOfLongestSubstring = calc [] Set.empty 0 0 
  where
    calc :: StringQueue -> Set.Set Char -> Length -> Length -> String -> Length
    calc _ _ _ maxLen [] = maxLen
    calc strQueue charSet currLen maxLen (currChar:pendingStr) =
      if currChar `Set.member` charSet
        then let (outChars, (_:newStrQueue')) = break (== currChar) newStrQueue
                 outChars' = currChar:outChars
                 newCharSet = foldr (\outc cset -> Set.delete outc cset) charSet outChars'
                 newCurrLen = currLen - length outChars'
             in calc newStrQueue' newCharSet newCurrLen maxLen pendingStr
        else let newCharSet = Set.insert currChar charSet
                 newCurrLen = currLen + 1
             in calc newStrQueue newCharSet newCurrLen (max newCurrLen maxLen) pendingStr
      where
        newStrQueue :: StringQueue
        newStrQueue = strQueue ++ [currChar] 