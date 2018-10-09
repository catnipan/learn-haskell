compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
-- compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

tenDivide :: (Floating a) => a -> a
tenDivide = (/) 10

-- 中缀函数的不完全调用
-- 返回一个取一参数并将其补到缺少的那一端的函数
-- (/ 10) 2 = 0.2
-- 因为 / 是中缀函数，(/ 10)相当于空了第一个位置，同理
-- (`compare` 3) 4 = GT
-- (`compare` 3) 相当于 \ x -> (x `compare` 3)

-- (/) 10 2 = 5.0
-- (compare 3) 4 = LT
-- 此时不是中缀函数
-- x / y 即 (/) x y

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- 此时用括号 (a -> a)，我们需要括号来“破坏”第二个 -> 的右结合性质

-- applyTwice (3:) [1] 为 [3, 3, 1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (zipWith' (*)) [[1,2,3], [3,5,6], [2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--   where g x y =  f y  x
-- flip' f y x = f x y
-- 使用 lambda
flip' f = \x y -> f y x

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
  let smallerSorted = quickSort' (filter (<=x) xs)
      biggerSorted = quickSort' (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0
-- 找出小雨 100000 的 3829 的最大的倍数

-- Collatz 序列
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

-- 1-100 作为起始数的 Collatz 序列有多少长度大于15
numLongChains :: Int
-- numLongChains = length (filter isLong (map chain [1..100]))
--   where isLong xs = length xs > 15
-- 使用 lambda
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
-- sum' = foldl (+) 0
-- a.k.a
sum' = foldl1 (+)

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- 用 fold 来实现
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []
-- (\acc x -> x:acc)
-- 即 (\acc x -> (:) x acc)
-- 即 flip (:)
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)
-- product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldl (\acc x -> if p x then x:acc else acc ) []

-- 用 fold 可以实现 head 和 last，但是效率比模式匹配低
head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- 用 foldl 来实现 scanl
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- let b = [c] then we have
-- foldl :: ([c] -> a -> [c]) -> [c] -> [a] -> [c]
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f acc xs = foldl (\accs x -> accs ++ [(f (last accs) x)]) [acc] xs

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..])))

-- $ operator 拥有最低的优先级，右结合
-- ($) :: (a -> b) -> a -> b
-- f (g (z x)) 等于
-- f $ g $ z x
-- sum (filter (>10) (map (*2) [2..10])) 等于
-- sum $ filter (>10) $ map (*2) [2..10]

-- $ 作为 applyTo 使用
-- R.applyTo(42)(R.add(1))
-- ($ 42)(+1)
-- 中缀函数 ($ 42) 相当于差一个 f 参数
-- map ($ 3) [(4+), (10*), (^2), sqrt]


-- composition 右结合
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- 函数的复合运算：生成新的函数
-- f (g (z x)) 等于 (f.g.z)x