-- do 表示法可以用于任何 monad
-- Just 3 >>= (\x -> Just (show x ++ "!"))
-- Just "3!"
-- Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Just "3!"

foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- 等价于
foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  -- z <- Nothing 如果在任何一步 Nothing，结果都是 Nothing
  Just (show x ++ y)

-- do notation 中的 <- 就相当于
-- (Just 3) >>= \x -> Just (x+1)
-- 取出右边函数 x 参数的过程

-- Just 9 >>= (\x -> Just (x > 8))
-- Just True

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

-- 代表“序列”
-- 看起来像命令式
-- 每一步的值依赖于前一步的值

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

justH' :: Maybe Char
justH' = Just "hello" >>= (\(x:xs) -> Just x)

wopop :: Maybe Char
wopop = do
  (x:xs) <- Just ""
  return x

-- wopop
-- Nothing
-- (x:xs) 模式匹配时调用 Monad 的 fail 函数，Maybe 定义的 fail 为
-- fail _ = Nothing