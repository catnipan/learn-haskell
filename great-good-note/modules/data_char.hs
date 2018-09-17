import Data.Char
import Data.Function (on)
import Data.List (groupBy)

-- isControl
-- isSpace
-- isLower
-- isAlpha
-- isAlphaNum
-- isPrint
-- isDigit
-- isOctDigit
-- isHexDigit
-- isLetter
-- isMark
-- isNumber
-- isPunctuation
-- isSymbol
-- isSeperator
-- isAscii
-- isLatin1
-- isAsciiUpper
-- isAsciiLower

words' :: String -> [String]
words' str = filter (/=" ") $ groupBy ((==) `on` isSpace) str

-- generalCategory :: Char -> GeneralCategory
-- LowercaseLetter :: GeneralCategory
-- generalCategory 'a' == LowercaseLetter
-- True

-- toUpper, toLower, toTitle
-- digitToInt :: Char -> Int
-- digitToInt 'F'
-- 15
-- intToDigit :: Int -> Char
-- intToDigit 15
-- 'f'

-- ord :: Char -> Int
-- chr :: Int -> Char
-- ord, chr

caesarEncode :: Int -> String -> String
caesarEncode shiftSize msg =
  map chr $ map (+ shiftSize) $ map ord msg
  
caesarDecode :: Int -> String -> String
caesarDecode shiftSize msg =
  caesarEncode (negate shiftSize) msg