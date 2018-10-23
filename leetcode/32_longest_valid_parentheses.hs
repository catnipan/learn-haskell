-- Given a string containing just the characters '(' and ')', find the length of the longest valid (well-formed) parentheses substring.

-- Example 1:

-- Input: "(()"
-- Output: 2
-- Explanation: The longest valid parentheses substring is "()"
-- Example 2:

-- Input: ")()())"
-- Output: 4
-- Explanation: The longest valid parentheses substring is "()()"

import Data.Foldable(foldrM)
import Data.List(maximumBy)
import Data.Function(on)

data BrState = BrState {
  isRecorded :: Bool,
  getBrStack :: String,
  getBrStr :: String,
  getBrLength :: Int
} | EmptyBrState deriving (Show)

-- foldrM :: (Char -> BrState -> [BrState]) -> BrState -> [Char] ->[BrState]

isValidEndingState :: BrState -> Bool
isValidEndingState (BrState _ "" _ _) = True
isValidEndingState EmptyBrState = True
isValidEndingState _ = False

getBrStr' :: BrState -> String
getBrStr' EmptyBrState = ""
getBrStr' brs = getBrStr brs

getBrLength' :: BrState -> Int
getBrLength' EmptyBrState = 0
getBrLength' brs =  getBrLength brs

record :: BrState -> BrState
record bs = bs { isRecorded = True }

longestValidParentheses :: String -> String
longestValidParentheses brStr =
  getBrStr' . getMaxLength . filter isValidEndingState $ allEndingBrStates
  where
    getMaxLength = maximumBy (compare `on` getBrLength')
    allEndingBrStates = foldrM genNewBrs EmptyBrState brStr
    genNewBrs :: Char -> BrState -> [BrState]
    genNewBrs _ rdBrs@(BrState True _ _ _) = [rdBrs]
    genNewBrs '(' EmptyBrState = [EmptyBrState]
    genNewBrs ')' EmptyBrState = [BrState False ")" ")" 1, EmptyBrState]
    genNewBrs ')' brs@(BrState False brStack brStr brLen) =
      [BrState False (')':brStack) (')':brStr) (brLen + 1),
      record brs,
      EmptyBrState]
    genNewBrs '(' brs@(BrState False [] _ _) = [record brs, EmptyBrState]
    genNewBrs '(' (BrState False (')':brStack) brStr brLen) =
      [BrState False brStack ('(':brStr) (brLen + 1)]
