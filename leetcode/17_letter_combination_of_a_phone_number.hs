-- Given a string containing digits from 2-9 inclusive, return all possible Char combinations that the number could represent.

-- A mapping of digit to Chars (just like on the telephone buttons) is given below. Note that 1 does not map to any Chars.

-- Example:

-- Input: "23"
-- Output: ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"].
-- Note:

-- Although the above answer is in lexicographical order, your answer could be in any order you want.

import qualified Data.Map as Map
import Data.Foldable(foldrM)

type Digit = Char
type Error = String

digitCharMap :: Map.Map Char [Char]
digitCharMap = Map.fromList [('2',"abc"),('3',"def"),('4',"ghi"),('5',"jkl"),('6',"mno"),('7',"pqrs"),('8',"tuv"),('9',"wxyz")]

letterCombinations :: [Digit] -> Either Error [String]
letterCombinations digitChars = foldrM foldFunc [""] digitChars
  where
    foldFunc :: Digit -> [String] -> Either Error [String]
    foldFunc dgt strs =
      case Map.lookup dgt digitCharMap of
        Nothing -> Left $ "Parse Error: " ++ [dgt] ++ " . Digit must be 2~9"
        Just chars -> Right $ do
          str <- strs
          char <- chars
          return $ char:str

-- foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
-- foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
-- b is [String]
-- m is Either Error
-- a is Digit
-- foldrM :: (Digit -> [String] -> Either Error [String]) -> [String] -> [Digit] -> Either Error [String]