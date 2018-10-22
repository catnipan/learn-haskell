-- Implement strStr().

-- Return the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.

-- Example 1:

-- Input: haystack = "hello", needle = "ll"
-- Output: 2
-- Example 2:

-- Input: haystack = "aaaaa", needle = "bba"
-- Output: -1
-- Clarification:

-- What should we return when needle is an empty string? This is a great question to ask during an interview.

-- For the purpose of this problem, we will return 0 when needle is an empty string. This is consistent to C's strstr() and Java's indexOf().

tails' :: [a] -> [[a]]
tails' xxs@(x:xs) = xxs:(tails' xs)
tails' [] = [[]]

isPrefixOf' :: String -> String -> Maybe Bool
(x:xs) `isPrefixOf'` (y:ys)
  | x /= y = Just False
  | otherwise = xs `isPrefixOf'` ys
_ `isPrefixOf'` [] = Nothing
[] `isPrefixOf'` _ = Just True

strStr :: String -> String -> Maybe Int
strStr haystack needle = getFirstPrefixMatch $ zip [0..] (tails' haystack)
  where
    getFirstPrefixMatch ((idx,hsStartFromIdx):rest) =
      let isPrefix = needle `isPrefixOf'` hsStartFromIdx
      in case isPrefix of
          Nothing -> Nothing
          Just True -> Just idx
          Just False -> getFirstPrefixMatch rest