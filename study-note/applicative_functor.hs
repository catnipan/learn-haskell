fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
fmap :: (a -> b) -> Maybe a -> Maybe b
-- 这种情况下
(++) :: [Char] -> [CHar] -> [Char]
-- a 是 [Char]
-- b 是 [Char] -> [Char]
-- 所以结果是 Maybe ([Char] -> [Char])

fmap compare (Just 'a') :: Maybe (Char -> Ordering)

fmap compare "A LIST OF CARDS" :: [Char -> Ordering]
compare :: (Ord a) => a -> a -> Ordering
fmap :: (a -> b) -> [a] -> [b]
-- a 是 Char，b 是 Char -> Ordering

-- 使用多参数的 f 来 map over 一个 Functor
-- 会得到一个“包含”类型为函数的 Functor，如
-- let a = fmap (*) [1,2,3,4]
-- fmap (\f -> f 9) a 调用这个函数

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- 要满足 Applicative typeclass 必须先满足 Functor typeclass

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something

pure (+) <*> Nothing <*> Just 5
-- Nothing
pure (+) <*> Just 3 <*> Just 5
-- Just 8
-- 任意多参数如果有 1 个为 Nothing，结果就是 Nothing

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f (<$>) x = fmap f x
-- 其实也就是
pure f <*> x

-- 这样上面的可以写成
(*) <$> Nothing <*> Just 5
(*) <$> Just 3 <$> Just 5

(++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"
-- 和
(++) "johntra" "volta"
-- "johntravolta"
-- 结构相似，只不过 Functor 版本多了计算语境

-- List 作为 Applicative Functor
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

pure "Hey" :: Maybe String
-- Just "Hey"
pure "Hey" :: [String]
-- ["Hey"]

-- [] 作为 Applicative Functor 时，
(<*>) :: [a -> b] -> [a] -> [b]

[(*0), (+100), (^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]

[(+),(*)] <*> [1,2] <*> [3,4]
-- [(+),(*)] <*> [1,2] 为
-- [(+1),(+2),(*1),(*2)]
-- 再 <*> [3,4] 为
-- [(+1) 3, (+1) 4, (+2) 3, (+2) 4, (*1) 3, (*2) 4, (*2) 3, (*2) 4]
-- [4,5,5,6,3,4,6,8]

(++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

[x*y | x <- [2,5,10], y <- [8,10,11]]
-- 等价于
(*) <$> [2,5,10] <*> [8,10,11]
pure (*) <*> [2,5,10] <*> [8,10,11]

-- IO 作为 Applicative Functor
instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

-- 等价于
myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

main = do
  a <- myAction'
  putStrLn $ "The two lines concatenated turn out to be: " ++ a

-- (->) r 作为 Applicative Functor
instance Applicative ((->) r) where
  pure = const
  f <*> g = \x -> f x (g x)

pure :: Applicative f => a -> f a
-- 这里 f 为 (->) r，有
pure :: a -> ((->) r a)
pure :: a -> (r -> a)

pure 3 "blah"
-- 3

(+) <$> (+3) <*> (*100) $ 5
-- 将两个 Applicative Functor 喂给 <*> 我们可以得到一个新的 Applicative Functor
(+) :: Num a => a -> a -> a
(+3) :: Num a => a -> a
(+) <$> (+3) :: Num a => a -> a -> a
(*100) :: Num a => a -> a

(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- 首先 (\x y z -> [x,y,z]) 升格为
-- \n -> \x y z -> [x,y,z]
-- (\x y z -> [x,y,z]) <$> (+3) 为
-- \n -> (((\n -> \x y z -> [x,y,z]) n) (+3) n)
-- \n -> ((\x y z -> [x,y,z]) (n + 3))
-- \n -> (\y z -> [n+3, y, z])
-- 这个函数继续 <*> (*2) 为
-- \n -> ((这个函数) n) (n + 2)
-- \n -> ((\n -> (\y z -> [n+3, y, z])) n) (n+2)
-- \n -> (\y z -> [n+3, y, z]) (n+2)
-- \n -> (\z -> [n+3, (n+2), z])
-- 真个函数继续 <*> (/2) 为
-- \n -> (这个函数) n (n / 2)
-- \n -> (\n -> (\z -> [n+3, (n+2), z])) n (n / 2)
-- \n -> \z -> [n+3, (n+2), z])) (n / 2)
-- \n -> [n+3, (n+2), n/2]
-- 再 $ 5 即执行

-- :m + Control.Applicative
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- pure (*2) <*> ZipList [1,5,10]
-- ZipList {getZipList = [2,10,20]}
-- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
-- [101,102,103]
-- getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- [('d','c','r'),('o','a','a'),('g','t','t')]

-- 使用 applicative style 方式操作 ZipList，可以实现任意数量的 zipWithN

-- :m + Control.Applicative
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftA2 (:) (Just 3) (Just [4])
(:) <$> Just 3 <*> Just [4]
-- Just [3,4]
(:) :: a -> [a] -> [a]
-- 对于 Maybe 这个 Applicative Functor 来说
-- (:) <$> Just 3
-- fmap (:) Just 3
-- Just ((:) 3)
-- 它再去 <*> Just [4]
-- Just ((:) 3 [4])
-- Just [3,4]

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA [Just 1, Just 2]
-- Just [1,2]
-- (:) <$> Just 1 <*> [Just 2]
-- (:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

sequenceA [(+3), (+2), (+1)] 3
-- [6,5,4]
sequenceA [a -> a]
-- 变为 a -> [a]

map (\f -> f 7) [(>4), (<10), odd]
-- [True,True,True]
-- 等价于
sequenceA [(>4),(<10),odd] 7
-- [True,True,True]

sequenceA [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- 等价于 [[x,y] | x <- [1,2,3], y <- [4,5,6]]

-- IO 的 sequenceA 等价于 sequence
sequenceA [getLine, getLine, getLine]

-- applicative functor law
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
