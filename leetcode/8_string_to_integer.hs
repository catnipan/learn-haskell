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
import Data.Char(isDigit)
import qualified Data.Map as Map

charDigitMap :: Map.Map Char Int32
charDigitMap = Map.fromList [('0',0),('1',1),('2',2),('3',3),('4',4),('5',5),('6',6),('7',7),('8',8),('9',9)]

myAtoi :: String -> Int32
myAtoi str =
  case (makeSign <*> (Just . convertDigitStr 0 $ restStr)) of
    (Just x) -> x
    Nothing -> 0
  where
    (makeSign, restStr) = handleSign . dropWhile (==' ') $ str
    convertDigitStr :: Int32 -> String -> Int32
    convertDigitStr i [] = i
    convertDigitStr i (char:rsChars) =
      case Map.lookup char charDigitMap of
        (Just ni) -> let nv = (i * 10 + ni)
                      in convertDigitStr (if nv < 0 then 0 else nv) rsChars
        Nothing -> i
    handleSign :: String -> (Maybe (Int32 -> Int32), String)
    handleSign str = 
      case head str of
        '+' -> (Just id, tail str)
        '-' -> (Just negate, tail str)
        s -> (if isDigit s then Just id else Nothing, str)