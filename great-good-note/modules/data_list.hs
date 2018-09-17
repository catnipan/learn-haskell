import Data.List
-- Data.List 中的一些函数

-- intersperse 0 [1,2,3]
-- [1,0,2,0,3]

-- intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,0,0,0,4,5,6,0,0,0]

-- transpose [[1,2,3],[4,5],[6]]
-- [[1,4,6],[2,5],[3]]

-- foldl',foldl1' 严格版本的折叠函数

-- concat [[3,4,5], [2,3,4], [2,1,1]]
-- [3,4,5,2,3,4,2,1,1]

-- concatMap func xs = concat $ map func xs

-- and [True, True, True]
-- True

-- or [False, False, False]
-- False

-- all func xs = and $ map func xs
-- any func xs = or $ map func xs

-- iterate :: (a -> a) -> a -> [a]
-- iterate (*2) 1 = [1, 2, 4, 8, 16, ...]

-- splitAt :: Int -> [a] -> ([a], [a])
-- splitAt 3 "heyman"
-- ("hey", "man")

-- takeWhile
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- 一旦不满足条件，就不再丢掉了
-- dropWhile (/=' ') "This is a sentence"
-- " is a sentence"

-- partition :: (a -> Bool) -> [a] -> ([a], [a])
-- span 同上
-- break 同上
-- partition 遍历，符合的放在第一个 List，不符合的在第二个 List
-- span 在条件首次为 False 时断开 List，满足条件的在后一个 List 里
-- break 在条件首次为 True 时断开 List
-- partition 
-- break pred = span (not . pred)

-- break (==4) [1,2,3,4,5,6,7]
-- ([1,2,3], [4,5,6,7])

-- group :: Eq a => [a] -> [[a]]
-- 临近相等的放进一个 List 里面

-- inits :: [a] -> [[a]]
-- inits "w00t"
-- ["", "w", "w0", "w00", "w00t"]
-- tails "w00t"
-- ["w00t", "00t", "0t", "t", ""]

-- inInfixOf, isPrefixOf, isSuffixOf
isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- elemIndex :: Eq a => a -> [a] -> Maybe Int
-- 4 `elemIndex` [1,2,3,4,5,6]
-- Just 3
-- 10 `elemIndex` [1,2,3,4,5,6]
-- Nothing

-- elemIndices :: Eq a => a -> [a] -> [Int]
-- ' ' `elemIndices` "Where are the spaces?"
-- [5,9,13]

-- findIndex (==4) [5,3,2,1,6,4]
-- Just 5

-- findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
-- [0,6,10,14]

-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
-- zip3, zip4, ...
-- zipWith3, zipWith4, ..

-- lines、unlines、words、unwords
-- lines :: String -> [String]
-- lines "first line\nsecond line\nthird line"
-- ["first line","second line","third line"]
-- unlines 是 lines 的反函数

-- delete :: Eq a => a -> [a] -> [a]
-- 删除第一个
-- delete 'h' "Hey there ghang!"
-- "hey tere ghang!"

-- (\\) :: Eq a => [a] -> [a] -> [a]
-- 差集运算 左 - 右
-- [1..10] \\ [2,5,9]
-- [1,3,4,6,7,8,10]

-- union :: Eq a => [a] -> [a] -> [a]
-- 并集运算
-- intersect :: Eq a => [a] -> [a] -> [a]
-- 交集运算

-- insert :: Ord a => a -> [a] -> [a]
-- insert 4 [1,2,5,6]
-- [1,2,4,5,6]
-- 插入到第一个大于它的之前，保持排序

-- length，take，drop，splitAt 和 replace 接收/返回 Int
-- genericLength, genericTake, genericDrop,
-- genericSplitAt, genericIndex, genericReplicate
-- 如 genericLength :: Num i => [a] -> i

-- nub, delete, union, intsect, group
-- nubBy, deleteBy, unionBy, intersectBy, groupBy
-- nubBy :: (a -> a -> Bool) -> [a] -> [a]
-- nub = nubBy (==) 等等

-- groupBy (\x y -> (x > 0) == (y > 0)) values
-- 按照元素是否都大于 0 分为一类
-- 这里可借用 Data.Function 中的 on
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- f `on` g = \x y -> f (g x) (g y)
-- (\x y -> (x > 0) == (y > 0)) 等价于
-- (==) `on` (> 0)

-- sort, insert, maximum, minimum
-- sortBy, insertBy, maximumBy, minimumBy
-- sort :: Ord a => [a] -> [a]
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- 这里 type a 就不需要为 Ord typeclass 了
-- sortBy (compare `on` length) [[1..10],[1..2],[3..6]]