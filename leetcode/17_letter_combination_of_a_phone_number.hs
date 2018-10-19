-- Given a string containing digits from 2-9 inclusive, return all possible Char combinations that the number could represent.

-- A mapping of digit to Chars (just like on the telephone buttons) is given below. Note that 1 does not map to any Chars.

-- Example:

-- Input: "23"
-- Output: ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"].
-- Note:

-- Although the above answer is in lexicographical order, your answer could be in any order you want.

import qualified Data.Map as Map
import Data.Foldable(foldrM)

type DigitString = String
type Error = String

digitCharMap :: Map.Map Char [Char]
digitCharMap = Map.fromList [('2',"abc"),('3',"def"),('4',"ghi"),('5',"jkl"),('6',"mno"),('7',"pqrs"),('8',"tuv"),('9',"wxyz")]

letterCombinations :: DigitString -> [[Char]]
letterCombinations digitsStr = foldrM foldMFunc "" dChars
  where
    dChars :: [[Char]]
    dChars = do
      d <- digitsStr
      return $ case Map.lookup d digitCharMap of
        Nothing -> []
        Just ls -> ls
    foldMFunc :: [Char] -> String -> [String]
    foldMFunc chars str = do
      char <- chars
      return $ char:str

-- Left $ "Parse Error: " ++ [d] ++ " . Digit must be 2~9"
-- TODO: Either provide error info
-- foldM foldMFunc ["ab"] ["cd", "ef"]
-- foldM :: (b -> a -> [b]) -> b -> [a] -> [b]
-- foldM :: (String -> [Char] -> [String]) -> String -> [[Char]] -> [String]