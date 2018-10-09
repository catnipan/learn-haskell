-- data Maybe a = Nothing | Just a
-- a 是个类型参数
-- 有了类型参数后 Maybe 为类型构造子，而非类型
-- Maybe Int，Maybe String 这样补上类型参数后才是类型

data Car a b c = Car {
  company :: a,
  model :: b,
  year :: c
} deriving (Show)

tellCar :: (Show a) => Car String String a -> String
tellCar (Car { company = cpy, model = mdl, year = yr }) = "This " ++ cpy ++ " " ++ mdl ++ " was made in " ++ show yr

-- 类型作为容器
-- 包含的类型并不影响它的行为时引入类型参数

data Vector a = Vector a a a deriving (Show)

vPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vPlus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vMult` m = Vector (i*m) (j*m) (k*m)

sMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `sMult` (Vector l m n) = i*l + j*m + k*n

-- 我们并没有在 data 声明中添加 Num 的类约束
-- 而是给函数加约束
-- Vector 即是类型构造子 Vector t
-- 又是值构造子 Vector i j k