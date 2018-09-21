-- import 必须在函数的定义之前
-- import Data.List

import Data.List (nub)
-- 只使用该 module 中的 nub 和 sort

-- import Data.List hiding (nub)
-- 隐藏 module 中的 nub

-- qualified import
-- import qualified Data.Map
-- 使用函数为 Data.Map.filter
-- import qualified Data.Map as M
-- 使用函数为 M.filter

-- 在 ghci 中装载
-- :m Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- 使用了 Data.List 中的
