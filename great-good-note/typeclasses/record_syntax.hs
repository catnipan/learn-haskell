data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

-- Record Syntax
-- 自动生成函数 flavor :: Person -> String

data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Show)

-- Record Syntax 自动生成的 show :: Car -> String 不一样
-- Car "Ford" "Mustang" 1967
-- Car {company = "Ford", model = "Mustang", year = 1967}

-- Car {company="Ford", model="Mustang", year=1967} 可以使用 key 不在乎顺序来构造