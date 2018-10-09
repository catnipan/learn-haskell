-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- >>= read as bind

-- Maybe as Monad
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- Just 3 `applyMaybe` \x-> Just (x+1)
-- Just 4
-- Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
-- Just "smile :)"
-- Nothing `applyMaybe` \x -> Just (x+1)
-- Nothing

-- Just 3 `applyMaybe` \x -> if x > 2 then Just (x+1) else Nothing
-- Just 4

class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b
  -- bind 函数，接收一个 monadic value，喂给一个接收普通值函数，返回一个 monadic value

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail msg = error msg

-- Applicative Functor 加入 haskell 较晚
-- 每个 monad 都是 applicative functor

-- return 等价于 pure

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing

-- return "What" :: Maybe String
-- Just "What"
-- Just 9 >>= \x -> return (x * 10)
-- Just 90
-- Nothing >>= \x -> return (x * 10)
-- Nothing