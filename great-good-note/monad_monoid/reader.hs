-- instance Monad ((->) r) where
--   return x = \_ -> x
--   h >>= f = \r -> f (h r) r

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- 在这里 m 是 ((->) r)
-- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)

-- h :: (r -> a)
-- f :: (a -> (r -> b))
-- h r :: a
-- f (h r) r :: b
-- \r -> f (h r) r:: (r -> b)

_addStuff :: Int -> Int
_addStuff = (+) <$> (*2) <*> (+10)

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

addStuff' :: Int -> Int
addStuff' x = let
  a = (*2) x
  b = (+10) x
  in a+b