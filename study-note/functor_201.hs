import Data.Char
import Data.List
-- functors 是一个计算语境 computational context
-- 如果一个 type constructor 要是 Functor 的 instance
-- 那他的 kind 必须是 * -> *

-- IO 的 kind 为 * -> *
-- 它也可以作为 functor

-- instance Functor IO where
--   fmap f io = do
--     res <- io
--     return (f res)

-- 这里
-- fmap :: (a -> b) -> IO a -> IO b

main1 = do
  line <- getLine
  let line' = reverse line
  putStrLn line'

-- 等价于

main2 = do
  line <- fmap reverse getLine
  putStrLn line

main3 = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

-- (->) :: TYPE q -> TYPE r -> *
-- (->) r 的 kind 满足 functor
-- instance Functor ((->) r) where
--   fmap f g = (\x -> f (g x))
-- 这里 fmap :: (a -> b) -> f a -> f b
-- 将 f 换成 (->) r 就是
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
-- 实际上，这等价于 function composition
-- fmap = (.)

-- fmap (*3) (+100) 1
-- 303
-- (*3) `fmap` (+100) $ 1
-- 303
-- (*3) . (+100) $ 1
-- 303

-- (*2) :: (Num b) => b -> b
-- fmap (*2) :: (Num b, Functor f) => f b -> f b

-- (replicate 3) :: a -> [a]
-- fmap (replicate 3) :: (Functor f) => f a -> f [a]

-- fmap (replicate 3) [1,2,3,4]
-- [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
-- fmap (replicate 3) (Just 4)
-- Just [4,4,4]
-- fmap (replicate 3) (Right "blah")
-- Right ["blah", "blah", "blah"]
-- fmap (replicate 3) Nothing
-- Nothing
-- fmap (replicate 3) (Left "foo")
-- Left "foo"

-- 这里 f 属于 Functor typeclass
-- 即可以是 Maybe, [], IO
-- 或者定义域固定的函数 (->) r

-- (fmap (++"!")) :: Functor f => f [Char] -> f [Char]
-- (fmap (++"!")) 可以接收哪些类型的参数？
-- IO [Char]
-- Maybe [Char]
-- [[Char]]
-- a -> [Char] like show

-- Functor Laws
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)

-- 对于 CMaybe，fmap id /= id
-- 前者的 counter 比原来多 1，后者比原来多 2
-- 对于 CMaybe，fmap (f . g) /= fmap f . fmap g
-- 前者的 counter 最后比原来多 1，后者比原来多 2

-- CMaybe 不遵守 Functor Laws，不是一个 Functor