import Control.Applicative((<|>))

digit :: Int -> String -> Maybe Int
digit _ [] = Nothing
digit i (c:_)
  | i > 9 || i < 0 = Nothing
  | otherwise = if [c] == show i
                  then Just i
                  else Nothing

-- (<|>) can be used to run two parsers in parallel. 

binChar :: String -> Maybe Int
binChar s = digit 0 s <|> digit 1 s
