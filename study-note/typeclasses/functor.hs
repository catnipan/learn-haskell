import qualified Data.Map as Map

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- 从下面的 fmap 的 f a 是一个具体类型可以看出
-- f 并不是一个具体类型，而是一个类型构造子（接受一个类型为参数，返回一个具体类型）

-- 如果
-- instance Functor XXX where
-- XXX 不是一个类型，而是一个类型构造子

-- [] 属于 Functor typeclass
-- map :: (a -> b) -> [a] -> [b]
-- f is []
-- f a is [a]
-- f b is [b]
-- instance Functor [] where
--   fmap = map

-- 可以当作盒子的类型可能就是一个 functor
-- 如 Maybe a 就是一个 “盒子”
-- instance Functor Maybe where
--   fmap f (Just x) = Just (f x)
--   fmap f Nothing = Nothing

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
-- Tree a 也是一个 “容器”
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) =
    Node (f x) (fmap f leftsub) (fmap f rightsub)

-- instance Functor (Either a) where
--   fmap f (Right x) = Right (f x)
--   fmap f (Left x) = Left x
-- data Either a b = Left a | Right b
-- fmap :: (a -> b) -> f a -> f b
-- 在此处是
-- fmap :: (x -> y) -> (Either a) x -> (Either a) y
