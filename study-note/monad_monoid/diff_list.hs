import Data.Monoid

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