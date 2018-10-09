module ValueConstructor_Shapes(
  Point(..), -- (..) 表示导出所有的值构造子
  Shape, -- 不导出任何构造子，只能使用 baseXXX 和 nudge 来构造
  surface,
  nudge,
  baseCircle,
  baseRect,
) where

-- data Bool = True | False
-- data 类型名称 = 值构造子 Value Constructor

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
-- 表示类型 Shape 可以是 Circle 也可以是 Rectangle

-- 定义值构造子时，可以在后面跟几个类型表示它包含值的类型
-- 立即有 Circle :: Point -> Float -> Shape
-- 构造子相当于普通函数
-- 可以 map (Circle 10 20) [4, 5, 6, 6]

-- 加上 deriving (Show) 自动将该类型置于 Show typeclass 之中

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 嵌套的模式匹配
-- 我们使用的模式匹配针对的都是值构造子
-- 之前我们匹配过 []、False 或 5，它们都是不包含参数的值构造子。

nudge :: Float -> Float -> Shape -> Shape
nudge a b (Circle (Point x y) r) = Circle (Point (x+a) (y+b)) r
nudge a b (Rectangle (Point x1 y1) (Point x2 y2)) = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

originPoint :: Point
originPoint = Point 0 0

baseCircle :: Float -> Shape
baseCircle r = Circle originPoint r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle originPoint (Point width height)