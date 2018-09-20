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