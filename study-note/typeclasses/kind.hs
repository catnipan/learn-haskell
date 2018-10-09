-- :k Int
-- Int :: *
-- 一个 * 代表这个类型是具体类型

-- :k Maybe
-- Maybe :: * -> *
-- Maybe 接受一个具体类型，返回一个具体类型

-- 类型是值的标签，而 kind 是类型的标签

-- :k Either
-- Either :: * -> * -> *

-- instance Functor (???) where
-- 中的 (???) 需要具有以下 kind
-- (???) :: * -> *
-- 比如 Maybe，或者是提供了一个具体类型的 Either

class Tofu t where
  tofu :: j a -> t a j
-- t 具有怎样的 kind ？
-- 因为 j a :: *
-- 有 j :: * -> *
-- 和 a :: *
-- 同样有 t a j :: *
-- 那么 t :: * -> (* -> *) -> *

data Frank a b = Frank { frankField :: b a } deriving (Show)
-- b a :: *
-- b :: * -> *
-- a :: *
-- Frank { frankField = Just "HAHA" } :: Frank [Char] Maybe
-- Just "HAHA" :: b a
-- b 是 Maybe，a 是 [Char]
-- Frank { frankField = "YES" } :: Frank Char []
-- "YES" :: b a
-- b 是 []，a 是 Char

instance Tofu Frank where
  tofu x = Frank x
-- tofu (Just 'a') :: Frank Char Maybe
-- Frank {frankField = Just 'a'}
-- tofu ["HELLO"] :: Frank [Char] []
-- Frank {frankField = ["HELLO"]}

data Barry t k p = Barry { yabba :: p, dabba :: t k }

-- Barry 的 kind 为？
-- p :: *
-- t k :: *
-- t :: * -> *
-- k :: *
-- Barry :: (* -> *) -> * -> * -> *

-- 想要吧它定义为 Functor 的 instance ？
-- 要想提供一个 kind 为 * -> * 的
-- 我们需要 partially apply (* -> *) 和 * 两个参数

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
