-- Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

-- For example, given n = 3, a solution set is:

-- [
--   "((()))",
--   "(()())",
--   "(())()",
--   "()(())",
--   "()()()"
-- ]

data BrState = BrState { getBrStr :: String, getRestLb :: Int, getRestRb :: Int } deriving (Show)

generateParenthesis :: Int -> [String]
generateParenthesis n = map getBrStr $ iterate (>>= genNewBrStates) [(BrState "" n n)] !! (n*2)
  where
    genNewBrStates :: BrState -> [BrState]
    genNewBrStates (BrState brStr restLb restRb) = apRb ++ apLb
      where apLb = if (restLb - 1 < restRb || restLb - 1 < 0) 
                    then [] else [BrState ('(':brStr) (restLb - 1) restRb]
            apRb = if (restRb - 1 < 0)
                    then [] else [BrState (')':brStr) restLb (restRb - 1)]