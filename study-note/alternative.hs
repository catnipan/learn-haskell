import Control.Applicative((<|>))
import Control.Monad(guard)

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

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
  | y /= 0 = Just (x `div` y)
  | otherwise = Nothing

safeDiv' :: Int -> Int -> Maybe Int
-- safeDiv' x y = do
--   guard (y /= 0)
--   return (x `div` y)
safeDiv' x y = (guard (y /= 0)) >>= (\_ -> return (x `div` y))

-- instance Alternative Maybe where
--   empty = Nothing
--   Nothing <|> r = r
--   l       <|> _ = l
-- guard           :: (Alternative f) => Bool -> f ()
-- guard True      =  pure ()
-- guard False     =  empty
-- guard True = Just ()
-- guard False = Nothing