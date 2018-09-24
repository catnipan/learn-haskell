import qualified Data.Foldable as F

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

-- 如果 instance Foldable (Tree a) where 我们要定义 foldMap
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
