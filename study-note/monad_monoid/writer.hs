import Data.Monoid
import Control.Monad.Writer

isBigBang :: Int -> Bool
isBigBang x = x > 9

isBigBang' :: Int -> (Bool, String)
isBigBang' x = (x > 9, "Compared gang size to 9.")

-- 把一个 (3, "Smallish gang.") 喂给 isBigBang
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- (3, "Smallish gang.") `applyLog` isBigBang'
-- (False,"Smallish gang.Compared gang size to 9.")
-- ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
-- (5,"Got outlaw name.Applied length.")

-- String 可以换为 (Monoid m) => m
-- ++ 改为 Monoid 通用的 `mappend`
applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- ("beans", Sum 10) `applyLog'` addDrink
-- ("milk",Sum {getSum = 35})
-- ("dogmeat", Sum 5) `applyLog'` addDrink `applyLog'` addDrink
-- ("beer",Sum {getSum = 65})

-- The Writer Type
-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance (Monoid w) => Monad (Writer w) where
--   return x = Writer (x, mempty)
--   (Writer (x,v)) >>= f =
--     let (Writer (y,nv)) = f x
--     in Writer (y, v `mappend` nv)
  
-- runWriter (return 3 :: Writer String Int)
-- (3,"")
-- runWriter (return 3 :: Writer (Sum Int) Int)
-- (3,Sum {getSum = 0})
-- runWriter (return 3 :: Writer (Product Int) Int)
-- (3,Product {getProduct = 1})

-- Writer 第一个类型参数为 Monoid 即上面对应的 log
-- 根据类型 mempty 分别是 "", Sum {getSum = 0}, Product {getProduct=1}

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got nubmer: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  tell ["wait for another operand"]
  b <- logNumber 5
  tell ["gonna get result"]
  return (a*b)

-- runWriter $ logNumber 3
-- (3,["Got nubmer: 3"])
-- runWriter multWithLog
-- (15,["Got nubmer: 3","wait for another operand","Got nubmer: 5","gonna get result"])

gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd'' b (a `mod` b)

-- fst . runWriter $ gcd'' 8 3
-- 1
-- mapM_ putStrLn $ snd $ runWriter (gcd'' 8 3)
-- 8 mod 3 = 2
-- 3 mod 2 = 1
-- 2 mod 1 = 0
-- Finished with 1

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      return result
-- 和 gcd'' 比起来没有效率，因为这里 ++ 是右结合来计算的
-- gcd'' 8 3 里的 log 是
-- ["8 mod 3 = 2"] ++ rest
-- gcdReverse 8 3 里的 log 是
-- rest ++ ["8 mod 3 = 2"]

-- list 在重复 append 时低效
-- 使用 difference list
-- 等价于 [1,2,3] 的 difference list 是 \xs -> [1,2,3] ++ xs
-- 等价于 [] 的 difference list 是 \xs -> [] + xs

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))
  
instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  
-- fromDiffList (toDiffList [1,2,3,4] <> toDiffList [1,2,3])
-- [1,2,3,4,1,2,3]

-- 通过 DiffList 改进版的 gcdReverse'
gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
  | b == 0 = do
      tell (toDiffList ["Finished with " ++ show a])
      return a
  | otherwise = do
      result <- gcdReverse' b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result

-- mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse' 110 34
-- Finished with 2
-- 8 mod 2 = 0
-- 34 mod 8 = 2
-- 110 mod 34 = 8

-- 对比 DiffList 和普通 list

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
  tell ["0"]
finalCountDown x = do
  finalCountDown (x-1)
  tell [show x]

-- finalCountDown 500000
-- result `append` [500000]

finalCountDown' :: Int -> Writer (DiffList String) ()
finalCountDown' 0 = do
  tell (toDiffList ["0"])
finalCountDown' x = do
  finalCountDown' (x-1)
  tell (toDiffList [show x])

-- 对比
-- mapM_ putStrLn . snd . runWriter $ finalCountDown 500000
-- mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown' 500000