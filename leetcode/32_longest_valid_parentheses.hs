-- Given a string containing just the characters '(' and ')', find the length of the longest valid (well-formed) parentheses substring.

-- Example 1:

-- Input: "(()"
-- Output: 2
-- Explanation: The longest valid parentheses substring is "()"
-- Example 2:

-- Input: ")()())"
-- Output: 4
-- Explanation: The longest valid parentheses substring is "()()"

import Control.Monad.Writer
import Data.Foldable(foldrM)

data Rb = Rb
instance Show Rb where
  show Rb = "*"

data BrState = BrState {
  getBrStack :: [Rb], -- there is only ')' in stacks
  getBrStr :: String
} deriving (Show)

data BrRecord = BrRecord { getRecordBrStrs :: [String], getRecordLength :: Int } deriving (Show, Eq)

instance Semigroup BrRecord where
  fsR@(BrRecord brs len) <> lsR@(BrRecord brs' len')
    | len < len' = lsR
    | len == len' = BrRecord (brs <> brs') len
    | len > len' = fsR

instance Monoid BrRecord where
  mempty = BrRecord [] 0

emptyBrState :: BrState
emptyBrState = BrState [] ""

toRecord :: BrState -> [BrRecord]
toRecord (BrState [] brStr) = [BrRecord [brStr] (length brStr)]
toRecord _ = []

longestValidParentheses :: String -> BrRecord
longestValidParentheses brStr = snd . runWriter . foldrM foldFunc (return emptyBrState) $ brStr
  where
    foldFunc :: Char -> [BrState] -> Writer BrRecord [BrState]
    foldFunc nBrChar brSs = do
      let newBrs = brSs >>= (genNewBrs nBrChar)
          newRecords = newBrs >>= toRecord
      if null newRecords 
        then return $ emptyBrState:newBrs
        else do
          tell $ mconcat newRecords
          return newBrs
    genNewBrs :: Char -> BrState -> [BrState]
    genNewBrs '(' (BrState (Rb:brStack) brStr) = [BrState brStack ('(':brStr)]
    genNewBrs '(' (BrState [] _) = []
    genNewBrs ')' (BrState brStack brStr) = [BrState (Rb:brStack) (')':brStr)]
