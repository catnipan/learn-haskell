-- Implement atoi which converts a string to an integer.

-- The function first discards as many whitespace characters as necessary until the first non-whitespace character is found. Then, starting from this character, takes an optional initial plus or minus sign followed by as many numerical digits as possible, and interprets them as a numerical value.

-- The string can contain additional characters after those that form the integral number, which are ignored and have no effect on the behavior of this function.

-- If the first sequence of non-whitespace characters in str is not a valid integral number, or if no such sequence exists because either str is empty or it contains only whitespace characters, no conversion is performed.

-- If no valid conversion could be performed, a zero value is returned.

-- Note:

-- Only the space character ' ' is considered as whitespace character.
-- Assume we are dealing with an environment which could only store integers within the 32-bit signed integer range: [−231,  231 − 1]. If the numerical value is out of the range of representable values, INT_MAX (231 − 1) or INT_MIN (−231) is returned.
-- Example 1:

-- Input: "42"
-- Output: 42
-- Example 2:

-- Input: "   -42"
-- Output: -42
-- Explanation: The first non-whitespace character is '-', which is the minus sign.
--              Then take as many numerical digits as possible, which gets 42.
-- Example 3:

-- Input: "4193 with words"
-- Output: 4193
-- Explanation: Conversion stops at digit '3' as the next character is not a numerical digit.
-- Example 4:

-- Input: "words and 987"
-- Output: 0
-- Explanation: The first non-whitespace character is 'w', which is not a numerical 
--              digit or a +/- sign. Therefore no valid conversion could be performed.
-- Example 5:

-- Input: "-91283472332"
-- Output: -2147483648
-- Explanation: The number "-91283472332" is out of the range of a 32-bit signed integer.
--              Thefore INT_MIN (−231) is returned.

import Data.Int(Int32)

data IntParseError = InvalidDigitChar | IntOverflow | IntUnderflow
data Sign = Positive | Negative

charToDigit :: Char -> Maybe Int32
charToDigit '0' = Just 0
charToDigit '1' = Just 1
charToDigit '2' = Just 2
charToDigit '3' = Just 3
charToDigit '4' = Just 4
charToDigit '5' = Just 5
charToDigit '6' = Just 6
charToDigit '7' = Just 7
charToDigit '8' = Just 8
charToDigit '9' = Just 9
charToDigit _ = Nothing

myAtoi :: String -> Int32
myAtoi str =
  case (convertDigitStr (Right 0) $ restStr) of
    (Right v) -> v
    (Left IntUnderflow) -> (minBound :: Int32)
    (Left IntOverflow) -> (maxBound :: Int32)
    _ -> 0
  where
    convertDigitStr :: Either IntParseError Int32 -> String -> Either IntParseError Int32 
    convertDigitStr v [] = v
    convertDigitStr (Left e) _ = (Left e)
    convertDigitStr (Right v) (char:rsChars) =
      case charToDigit char of
        Just nv -> convertDigitStr (testIntFlow $ v * 10 + makeSign nv) rsChars
        Nothing -> (Right v) -- right invalid char ignored
    testIntFlow :: Int32 -> Either IntParseError Int32
    makeSign :: Int32 -> Int32
    (testIntFlow, makeSign) = case sign of
      Positive -> (\i -> if i < 0 then Left IntOverflow else Right i, id)
      Negative -> (\i -> if i > 0 then Left IntUnderflow else Right i, negate)
    (sign, restStr) = handleSign $ dropWhile (==' ') str
    handleSign :: String -> (Sign, String)
    handleSign ('+':ss) = (Positive, ss)
    handleSign ('-':ss) = (Negative, ss)
    handleSign ss = (Positive, ss)