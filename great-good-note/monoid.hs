import Data.Monoid
-- typeclass 一些 type 所共有的行为

-- 幺半群 Monoid
-- 二元运算遵守结合律
-- 有一个单位元

-- Associativity
-- For all a, b and c in S, the equation (a • b) • c = a • (b • c) holds.

-- Identity element
-- There exists an element e in S such that for every element a in S, the equations e • a = a • e = a hold.

-- :m + Data.Monoid
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty -- mconcat 的缺省实现

-- 只有具体类型才能成为 Monoid 的 instance
-- mempty，一个 polymorphic 的常数，表示该幺半群中的单位元
-- mappend，该幺半群中的二元运算
-- mconcat，通过 foldr 不断调用该二元运算，使一个列表的值成为一个值

-- type m 是 Monoid typeclass 我们要保证
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- Lists are monoids

instance Monoid [a] where
  mempty = []
  mappend = (++)

-- 5 + 0 = 5
-- 0 + 5 = 5
-- (1 + 2) + 3 = 1 + (2 + 3)
-- 有 (Num, +, 0) 构成一个幺半群

-- 5 * 1 = 5
-- 1 * 5 = 5
-- (1 * 2) * 3 = 1 * (2 * 3)
-- 有 (Num, *, 1) 构成一个幺半群

-- 我们可以使用 newtype 包一下分别实现

-- :m + Data.Monoid
newtype Product a  = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)
-- 这里我们把单位元 empty 设为 1
-- 把幺半群二元运算设为 *

-- getProduct $ Product 3 `mappend` Product 9
-- 27
-- getProduct $ Product 3 `mappend` mempty
-- 3
-- getProduct . mconcat . map Product $ [3,4,2]
-- 24

newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

-- getSum $ Sum 2 `mappend` Sum 5
-- 7

-- (Bool, ||, False) 构成一个幺半群
newtype Any = Any { getAny :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

-- getAny $ Any True `mappend` Any False
-- True

-- (Bool, &&, True) 构成一个幺半群
newtype All = All { getAll :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)

-- getAll . mconcat . map All $ [True, True, True]
-- True

-- (Ordering, 如下mappend, EQ) 构成一个幺半群
instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT

-- 字典序
-- 这里的 mappend 不符合交换律

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend` (x `compare` y)

-- 如果加逻辑
lengthCompare'' :: String -> String -> Ordering
lengthCompare'' x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend`
                      (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")

-- Maybe a 作为 Monoid，需要 type a 属于 Monoid typeclass
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `append` Nothing = m
  Just m1 `append` Just m2 = Just (m1 `mappend` m2)

-- Just (Sum 3) `mappend` Just (Sum 4)
-- Just (Sum {getSum = 7})

-- :m + Data.Monoid
newtype First a = First { getFirst :: Maybe a }
  deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing `mappend` x = x

-- 相对应的有 Last
-- 保留最后一个 Just xxx
-- _ `mappend` Last (Just x) = Last (Just x)