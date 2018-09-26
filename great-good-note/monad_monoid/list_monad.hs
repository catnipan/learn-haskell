-- instance Monad [] where
--   return x = [x]
--   xs >>= f = concat . map f $ xs
--   fail _ = []

-- [3,4,5] >>= \x -> [x,-x]
-- [3,-3,4,-4,5,-5]
-- [] >>= \x -> ["bad","mad","rad"]
-- []
-- [1,2,3] >>= \x -> []
-- []

-- [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1,2]
  ch <- ['a','b']
  return (n,ch)

-- [(n,ch) | n <- [1,2], ch <- ['a','b']]
-- list comprehension 是 >>= 的一个语法糖

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- guard (5 > 2) :: [()]
-- [()]
-- guard (1 > 2) :: [()]
-- []

-- [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- [7,17,27,37,47]
-- 当 x 为 6 时，guard 函数返回 mzero 即 []
-- (\x -> ([] >> return x)) 6
-- (\x -> ([] >> \_ -> return x)) 6
-- (\x -> []) 6
-- []
-- concat 这个 [] 就相当于过滤掉了

-- 当 x 为 7 时，guard 函数返回 return ()
-- (\x -> (return () >> return x)) 7
-- (\x -> ([()] >>= (\_ -> return x))) 7
-- (\x -> [x]) 7
-- [7]

-- guard (5 > 2) >> return "cool" :: [String]
-- ["cool"]
-- guard (1 > 2) >> return "cool" :: [String]
-- []
-- guard 成功是一个 [()]，空的 tuple 不起作用，
-- 使右边的继续进行
-- 失败则是一个空的 []
-- 本质在于 [] >> 和 [()] >> 表现不一样

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x