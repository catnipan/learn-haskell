-- doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x * 2

conanO'Brien = "It's a-me, Conan O'Brien"

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Int 是有界的
-- Integer 没有

factorial :: Integer -> Integer
factorial n = product [1..n]
-- factorial 50 = 30414093201713378043612608166064768844377641568960512000000000000

-- Float 单精度浮点数
circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double 双精度浮点数
circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- 类型首字母必须大写
-- head :: [a] -> a
-- 这里的 a 是类型变量

-- 如果一个 type 属于某个 typeclass
-- 那么实现了所需的行为/函数

-- Typeclass:
-- Eq Ord Show Read Enum Bounded Num

-- Enum 可枚举类型
-- Bounded 有成员 minBound、maxBound
-- minBound :: Bounded a => a

-- maxBound :: (Bool, Int, Char) 为 (True,9223372036854775807,'\1114111')

-- if a is a Ord, then it must be a Eq.
-- if a is a Show, must implement function show
-- show :: Show a => a -> String

-- read :: Read a => String -> a
-- read 可以从 string parse 为该值的类型

-- Num typeclass 具有数字特征，比如支持乘法等
-- Integral typeclass 整数：包含 type Int、Integer
-- Floating typeclass 浮点类型：包含 type Float、Double