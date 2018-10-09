import qualified Data.Foldable as F
import Data.Monoid

-- 使用 Monoid 来帮助 fold 数据结构

-- F.foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- fold 任意属于 Foldable typeclass 的 type

-- F.foldl (+) 2 (Just 9)
-- 11
-- F.foldl (+) 2 Nothing
-- 2

-- fold 一个 Maybe a 时，Nothing 就像 []，Just xxx 就像 [xxx]

-- F.foldr (||) False (Just True)
-- True

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- 如果 instance Functor Tree where 我们可以 fmap 它

testTree :: Tree Int
testTree =
  Node 5 
    (Node 2
      (Node 1 Empty Empty)
      (Node 6 Empty Empty)
    )
    (Node 9
      (Node 8 Empty Empty)
      (Node 10 Empty Empty)
    )

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- fmap (+1) testTree

-- 如果 instance Foldable (Tree a) where 我们要定义 foldMap
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

-- 第一个函数用来 map over 这个 foldable 的结构
-- 得到一个包含 Monoid 的结构，再 mconcat 起来

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l
                            `mappend`
                            f x
                            `mappend`
                            F.foldMap f r

-- F.foldl (+) 0 testTree
-- 41
-- F.foldl (*) 1 testTree
-- 43200

-- F.foldMap (\x->[x]) testTree
-- [1,2,6,5,8,9,10]
-- 这里 (Monoid m) 是 [Int]
-- mappend 起来即是 [Int] ++ [Int]
-- 得到中序遍历序列

-- getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- False
-- 这里 (Monoid m) 是 Any 类型
-- mappend 起来是如果有一个 True 则为 True，全是 False 则为 False
-- getAny $ F.foldMap (\x -> Any $ x > 15) testTree
-- False
