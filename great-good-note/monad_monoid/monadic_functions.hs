import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

liftM' :: Monad m => (a1 -> r) -> m a1 -> m r
liftM' f m = m >>= (\x -> return (f x))

-- 用 do notation 写成
liftM'' f m = do
  x <- m
  return $ f x
-- 仅用 Monad 的性质可以实现 fmap ( Monad 一定满足 Functor )

-- liftM (*3) (Just 3)
-- Just 9
-- 等价于 fmap (*3) (Just 8)

-- runWriter $ liftM not $ writer (True, "chickpeas")
-- (False,"chickpeas")
-- 等价于 runWriter $ fmap not $ writer (True, "chickpeas")

type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)
push :: Int -> State Stack ()
push i = state $ \ls -> ((), i:ls)

-- runState (liftM (+100) pop) [1,2,3,4]
-- (101,[2,3,4])
-- 等价于 runState (fmap (+100) pop) [1,2,3,4]

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf m = do
  f <- mf
  x <- m
  return (f x)
-- 仅用 Monad 的性质可以实现 <*> ( Monad 一定满足 Applicative Functor )

-- Just (+3) <*> Just 4
-- Just 7
-- Just (+3) `ap` Just 4
-- Just 7
-- [(+1),(+2),(+3)] <*> [10,11]
-- [11,12,12,13,13,14]
-- [(+1),(+2),(+3)] `ap` [10,11]
-- [11,12,12,13,13,14]

-- 如果我们定义了一个 Monad XXX，可以定义它为 Functor 和 Applicative Functor
-- instance Functor XXX where
--   fmap = liftM
-- instance Applicative XXX where
--   <*> = ap
--   pure = return

join' :: (Monad m) => m (m a) -> m a
join' mm = do
  m <- mm
  m

-- m >>= f
-- 等价于
-- join (fmap f m)

-- join (Just (Just 9))
-- Just 9
-- join (Just Nothing)
-- Nothing
-- join Nothing
-- Nothing
-- join [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]
-- runWriter $ join (writer (writer (1,"aaa"),"bbb"))
-- (1,"bbbaaa")

-- runState (join' (state $ \s -> (push 10,1:2:s))) [0,0,0]
-- ((),[10,1,2,0,0,0])

filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' = undefined

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
      tell ["Keeping " ++ show x]
      return True
  | otherwise = do
      tell [show x ++ " is too large, throwing it away"]
      return False

-- runWriter . filterM keepSmall $ [9,1,5,2,10,3]
-- ([1,2,3],["9 is too large, throwing it away","Keeping 1","5 is too large, throwing it away","Keeping 2","10 is too large, throwingit away","Keeping 3"])

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True,False]) xs

-- powerset [1,2]
-- [[1,2],[1],[2],[]]

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' = undefined

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + 9)

-- foldM binSmalls 0 [2,8,3,1]
-- Just 36
-- foldM binSmalls 0 [2,11,3,1]
-- Nothing