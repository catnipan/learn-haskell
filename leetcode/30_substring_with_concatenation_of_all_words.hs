-- You are given a string, s, and a list of words, words, that are all of the same length. Find all starting indices of substring(s) in s that is a concatenation of each word in words exactly once and without any intervening characters.

-- Example 1:

-- Input:
--   s = "barfoothefoobarman",
--   words = ["foo","bar"]
-- Output: [0,9]
-- Explanation: Substrings starting at index 0 and 9 are "barfoor" and "foobar" respectively.
-- The output order does not matter, returning [9,0] is fine too.
-- Example 2:

-- Input:
--   s = "wordgoodstudentgoodword",
--   words = ["word","student"]
-- Output: []

import Data.List(permutations,isPrefixOf,tails)

findSubstring :: String -> [String] -> [Int]
findSubstring str words = foldr addIdx [] $ zip [0..] (tails str)
  where
    addIdx :: (Int, String) -> [Int] -> [Int]
    addIdx (currIdx,tstr) idxs = if any (`isPrefixOf` tstr) allConcatedWords
                        then currIdx:idxs
                        else idxs
    allConcatedWords :: [String]
    allConcatedWords = map mconcat $ permutations words
