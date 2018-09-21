import qualified Data.Map as Map

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- List 是一个类型构造子
-- List a 是一个具体类型
-- Cons a (List a) 是一个值构造子

data List' a = Empty' | Cons' { listHead :: a, listTail :: List' a } deriving (Show, Read, Eq, Ord)

infixr 5 :-:
-- fixity 声明
-- :-: operator 是右结合
-- 意味着 a :-: b :-: c 等价于 a :-: (b :-: c)
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
-- 使用值构造子 x :-: xs 做模式匹配

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr treeInsert EmptyTree
