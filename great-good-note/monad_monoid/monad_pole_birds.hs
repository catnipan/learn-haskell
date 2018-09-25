type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left+n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right+n)

-- landLeft 2 (landRight 1 (landLeft 1 (0, 0)))
-- 这里的 pattern 是，有一个初始数据，再不断用函数去作用它
-- 把初始数据写在左边，函数写在右边，左结合是最好的写法

x -: f = f x

-- 100 -: (*3) -: (+5) -: (/8)
-- 38.125

-- 上面的等价于
-- (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2
-- (3,1)

land' :: (Birds, Birds) -> Pole -> Maybe Pole
land' (ln,rn) (left,right)
  | abs (newLeft - newRight) < 4 = Just (newLeft, newRight)
  | otherwise = Nothing
  where newLeft = left + ln
        newRight = right + rn

landLeft' n = land' (n, 0)
landRight' n = land' (0, n)

-- return (0,0) >>= landRight' 2 >>= landLeft' 2 >>= landRight' 2
-- Just (2,4)
-- return (0,0) >>= landRight' 4 >>= landLeft' 4
-- Nothing

-- 为什么能串起来？
-- 因为每次计算之后，已计算部分的结构、未计算部分的结构、以及它们之间的联系，
-- 都和计算之前一样

-- 踩到香蕉皮，强行滑倒
banana :: Pole -> Maybe Pole
banana _ = Nothing

-- return (0,0) >>= landLeft' 1 >>= banana >>= landRight' 1
-- Nothing

-- banana：一个忽略前面的计算的函数
-- (>>) :: (Monad m) => m a -> m b -> m b
-- m >> n = m >>= const n
-- >>= banana 等价于 >> Nothing

-- Nothing >> Just 3
-- Nothing
-- Just 3 >> Just 4
-- Just 4

-- 使用 do notation
routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft' 2 start
  -- Nothing -- 丢出香蕉皮
  -- 没有用 <- 来绑定值，相当于 >> 函数
  -- _ <- Nothing
  second <- landRight' 2 first
  -- second <- Just (0,0) -- shadow second 变量为 (0,0)
  landLeft' 1 second
