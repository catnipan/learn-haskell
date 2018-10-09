import Data.Ratio
import Data.List (all)
import Control.Monad

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

instance Applicative Prob where
  pure x = Prob [(x,1%1)]
  (<*>) mf m = do
    f <- mf
    x <- m
    return (f x)

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where
  m >>= f = flatten (fmap f m)
  fail _ = Prob []

probList :: Prob Int
probList = Prob [(3, 1%2), (5, 1%4), (9, 1%4)]
-- fmap negate probList
-- Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}

thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [(Prob [('a',1%2),('b',1%2)],1%4),
    (Prob [('c',1%2),('d',1%2)],3%4)]

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2), (Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10), (Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (==Tails) [a,b,c])