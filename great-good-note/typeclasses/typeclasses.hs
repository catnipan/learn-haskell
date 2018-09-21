-- Prelude 中 typeclass Eq 是这样定义的

-- class Eq a where
--   (==) :: a -> a -> Bool
--   x == y = not (x /= y)
--   (/=) :: a -> a -> Bool
--   x /= y = not (x == y)

-- 如果一个类型 a 属于 typeclass Eq
-- 那么它需要实现以上几个函数
-- 相互调用 （minimal complete definition）
-- 我们只需要在 instance 中实现其中一个就好

data TrafficLight = Red | Yellow | Green

-- 为了让 type TrafficLight 属于 Eq 这个 typeclass
-- 我们需要提供 instance 如下

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- Red `elem` [Red, Green]
-- True

-- 为了让 type TrafficLight 属于 Show 这个 typeclass
-- 我们提供 instance 如下

instance Show TrafficLight where
  show Red = "[R]"
  show Yellow = "[Y]"
  show Green = "[G]"

-- 用 derive 来自动产生 show，会转换为字符串

-- instance Read TrafficLight where
--   read "R" = Red
--   read "Y" = Yellow
--   read "G" = Green

-- 对于有 parameter 的 type 而言，如 Maybe a
-- instance (Eq m) => Eq (Maybe m) where
  --   Just x == Just y = x == y
  --   Nothing == Nothing = True
  --   _ == _ = False
-- 我们要求 Maybe 的 type parameter m 属于 Eq typeclass
-- 这样才能让 Maybe m 也属于 Eq typeclass

-- 在 ghci 中查看一个 typeclass 有哪些 instance
-- :info Num
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--         -- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Double -- Defined in ‘GHC.Float’