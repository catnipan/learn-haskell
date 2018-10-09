getBmi :: (RealFloat a) => a -> a -> a
getBmi weight height = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, yo!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly"
  | bmi <= 30.0 = "You're fact! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
-- like if () { } else if () {} else if () {} else {}

getBmiTell :: (RealFloat a) => a -> a -> String
getBmiTell weight height = bmiTell (getBmi weight height)

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- 使用 where
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | bmi <= skinny = "A"
  | bmi <= normal = "B"
  | bmi <= fat = "C"
  | otherwise = "D"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)
      
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- where 里面不仅可以绑定名字（无参数的函数），还可以定义函数
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

-- let [bindings] in [expressions]
-- 在 [bindings] 中绑定的名字只在 [expressions] 中可见
-- let 中绑定名字要对齐一列
-- [let [bindings] in [expressions]] 这个本身是一个表达式，
-- 就像 [if [xx] then [expression] else [expression]] 一样是一个表达式

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
-- let 中绑定的名字在输出函数及限制条件中都可见
-- 但是 (w, h) <- xs 这里不可见，因为它在 let 绑定的前面
-- 此时忽略了 in 部分，因为名字的可见性已经定义好了，在[oo | xx, let __, oo, oo]

-- case [expression] of pattern -> result
--                      pattern -> result
--                      pattern -> result
-- 函数定义参数的模式匹配是 case 的语法糖
head' :: [a] -> a
head' [] = error "No head for empty list!"
head' (x:_) = x


head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty list!"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."