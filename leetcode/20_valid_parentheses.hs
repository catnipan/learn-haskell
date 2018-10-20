-- Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

--   An input string is valid if:
  
--   Open brackets must be closed by the same type of brackets.
--   Open brackets must be closed in the correct order.
--   Note that an empty string is also considered valid.
  
--   Example 1:
  
--   Input: "()"
--   Output: true
--   Example 2:
  
--   Input: "()[]{}"
--   Output: true
--   Example 3:
  
--   Input: "(]"
--   Output: false
--   Example 4:
  
--   Input: "([)]"
--   Output: false
--   Example 5:
  
--   Input: "{[]}"
--   Output: true

import Data.Foldable(foldrM)

type Error = String
data Bracket = Lb Int | Rb Int deriving (Show)
type BracketStack = [Bracket]

chToBr :: Char -> Either Error Bracket
chToBr '(' = Right (Lb 0)
chToBr ')' = Right (Rb 0)
chToBr '[' = Right (Lb 1)
chToBr ']' = Right (Rb 1)
chToBr '{' = Right (Lb 2)
chToBr '}' = Right (Rb 2)
chToBr '<' = Right (Lb 3)
chToBr '>' = Right (Rb 3)
chToBr ch = Left $ "Parse error: " ++ [ch] ++ " is not a valid bracket."

isValid :: String -> Either Error Bool
isValid strs = hdBrackets [] strs
  where
    hdBrackets :: BracketStack -> String -> Either Error Bool
    hdBrackets [] [] = Right True
    hdBrackets _ [] = Right False
    hdBrackets [] (s:str) = do
      nbr <- chToBr s
      case nbr of
        (Lb _) -> hdBrackets [nbr] str
        (Rb _) -> Right False
    hdBrackets bs@((Lb j):rbs) (s:str) = do
      nbr <- chToBr s
      case nbr of
        (Lb _) -> hdBrackets (nbr:bs) str
        (Rb i) -> if i == j then hdBrackets rbs str else Right False

-- TODO: refactor to State and Either monad combined style