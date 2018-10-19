-- Write a function to find the longest common prefix string amongst an array of strings.

-- If there is no common prefix, return an empty string "".

-- Example 1:

-- Input: ["flower","flow","flight"]
-- Output: "fl"
-- Example 2:

-- Input: ["dog","racecar","car"]
-- Output: ""
-- Explanation: There is no common prefix among the input strings.
-- Note:

-- All given inputs are in lowercase letters a-z.

longestCommonPrefix :: [String] -> String
longestCommonPrefix = foldr1 longestCommonPrefixOfTwo
  where
    longestCommonPrefixOfTwo :: String -> String -> String
    longestCommonPrefixOfTwo [] _ = []
    longestCommonPrefixOfTwo _ [] = []
    longestCommonPrefixOfTwo (s1:strs1) (s2:strs2)
      | s1 == s2 = s1:(longestCommonPrefixOfTwo strs1 strs2)
      | otherwise = []