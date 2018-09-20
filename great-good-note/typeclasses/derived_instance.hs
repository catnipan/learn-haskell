-- haskell 自动生成 Eq、Ord、Enum、Bounded、Show、Read 这几个 typeclass 的 instance

data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int
} deriving (Eq, Show, Read)

-- Eq 类型类
-- (Eq a) => 表示同属类型 a 的值可以判断是否等于相等
-- Show、Read 类型类
-- (Show a) => 表示类型 a 的值可以转换为字符串
-- (Read a) => 表示字符串可以转换为类型 a 的值

-- data Bool = False | True deriving (Ord)
-- data Maybe a = Nothing | Just a deriving (Ord)
-- 写在后面的构造子比写在前面的“大”，即有
-- True `compare` False
-- GT
-- Just 'd' `compare` Nothing
-- GT

-- 当使用同一个构造子时
data Test1 a = Test1 a deriving (Eq, Ord)
data Test2 a b = Test2 a b deriving (Eq, Ord)
-- 先比较 a，a 相同再比较 b
-- 如果 a 是不可比较类型报错，例如
-- Test1 (*3) `compare` Test1 (*4)
-- No instance for (Ord (Integer -> Integer))

data Day = Monday | Tuesday | WednesDay | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- 类型 Day 的所有的值构造子都是 nullary 的（没有参数），可以是 Enum typeclass
-- minBound :: Day
-- Monday
-- succ Monday
-- Tuesday
-- [Thursday .. Sunday]
-- [Thursday,Friday,Saturday,Sunday]
-- [minBound .. maxBound] :: [Day]
-- [Monday,Tuesday,WednesDay,Thursday,Friday,Saturday,Sunday]