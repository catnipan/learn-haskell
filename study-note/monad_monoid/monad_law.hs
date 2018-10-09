-- Monad Law

-- 1. Left Identity

-- return x >>= f 应该等于 f x
-- return 3 >>= (\x -> Just (x+1))
-- 等价于
-- (\x -> Just (x+1)) 3
-- 4
-- return "Wow" >>= (\x -> [x,x,x])
-- 等价于
-- (\x -> [x,x,x]) "Wow"
-- ["Wow","Wow","Wow"]

-- 2. Right Identity

-- m >>= return 应该等于 m
-- m >>= (\x -> return x)
-- m 如果是 Nothing, [] ，结果为本身
-- m 如果有东西，取出来再包进去，应该仍为原来的值
-- Just "move on up" >>= (\x -> return x)
-- Just "move on up"
-- [1,2,3,4] >>= (\x -> return x)
-- [1,2,3,4]
-- putStrLn "Wah!" >>= (\x -> return x)
-- Wah!
-- [] >>= (\x -> return x)
-- []

-- 3. Associativity
-- (m >>= f) >>= g 应该等价于
-- m >>= (\x -> f x >>= g)

-- monadic function 合成函数
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

-- 用合成函数来说
-- 1. 左单位元
-- f <=< return = f
-- 2. 右单位元
-- return <=< f = f
-- 3. 结合律
-- f <=< (g <=< h) 等价于 (f <=< g) <=< h

f x = [x, -x]
g x = [x*3, x*2]
h = f <=< g
-- h 3
-- [9,-9,6,-6]