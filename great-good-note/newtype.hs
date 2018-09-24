data ZipList' a = ZipList' [a]
data ZipList'' a = ZipList'' { getZipList'' :: [a] }

newtype ZipList''' a = ZipList''' { getZipList''' :: [a] }
-- 用 data 关键字来包一个类型的话
-- 在执行的时候会有一些包起来跟解开来的成本

-- 但使用 newtype 定义新类型只能定义一个值构造子

-- 使用 deriving 只能使用原来有的

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

-- CharList "this will be shown!"
-- CharList {getCharList = "this will be shown!"}
-- CharList "benny" == CharList "benny"
-- True

-- 我们有打包
-- CharList :: [Char] -> CharList
-- 解包
-- getCharList :: CharList -> [Char]

-- (a,b) 作为 Functor，要想 fmap 的时候是第一个怎么办
-- 使用 newtype
newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)

instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)

-- fmap :: (a -> b) -> Pair c a -> Pair c b

-- 这样就有
-- fmap (+4) $ Pair (3,4)
-- Pair {getPair = (7,4)}

-- On newtype laziness
-- head [3,4,5,undefined,2,undefined]
-- 因为惰性，列表中存在 undefined 却不会报错

data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- helloMe undefined
-- 会报错，因为 data 定义的类型可能有几个值构造子
-- 需要匹配 CoolBool 值构造子，一匹配就错了

newtype CoolBool' = CoolBool' { getCoolBool' :: Bool }

helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

-- helloMe' (CoolBool' True)
-- "hello"
-- helloMe' undefined
-- "hello"

-- 使用 newtype 的时候不会匹配，因为只有一个值构造子

-- newtype 关键字将现有的类型包成一个新的类型
-- 大部分情况是为了要让其可以是特定 typeclass 的 instance 而这样做