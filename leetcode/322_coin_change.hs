-- You are given coins of different denominations and a total amount of money amount. Write a function to compute the fewest number of coins that you need to make up that amount. If that amount of money cannot be made up by any combination of the coins, return -1.

-- Example 1:

-- Input: coins = [1, 2, 5], amount = 11
-- Output: 3 
-- Explanation: 11 = 5 + 5 + 1
-- Example 2:

-- Input: coins = [2], amount = 3
-- Output: -1
-- Note:
-- You may assume that you have an infinite number of each kind of coin.

import Data.Function(on)
type Coin = Int

newtype CoinGroup = CoinGroup (Int, [Coin]) deriving (Eq, Show)

instance Ord CoinGroup where
  compare (CoinGroup (a, _)) (CoinGroup (b, _)) = compare a b

cquot :: Int -> Int -> Int
x `cquot` y = ceiling $ (fromIntegral x :: Double) / (fromIntegral y :: Double)

coinChange :: [Coin] -> Coin -> CoinGroup
coinChange coins amount =
  case coinChange' coins amount of
    [] -> CoinGroup (-1, [])
    res -> minimum res
  where
    coinChange' :: [Coin] -> Coin -> [CoinGroup]
    coinChange' [] 0 = [CoinGroup (0, [])]
    coinChange' [] _ = []
    coinChange' (c:cs) amount = do
      takeC <- [0..(amount `cquot` c)]
      CoinGroup (cnt, cs) <- coinChange' cs (amount - takeC * c)
      return $ CoinGroup (cnt + takeC, (replicate takeC c) ++ cs)

-- TODO
-- use dynamic programming
-- performance needs improving

-- backtracking?