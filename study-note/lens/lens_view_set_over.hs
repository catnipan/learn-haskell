{-# LANGUAGE Rank2Types #-}

import Data.Functor.Identity
import Data.Functor.Const

type Lens b a = forall f.Functor f => (a -> f a) -> b -> f b

data Position = Position { positionX :: Double, positionY :: Double } deriving (Show)

-- xLens map a function to another function
xLens :: Lens Position Double
-- xLens :: Functor f => (Double -> f Double) -> Position -> f Position
xLens f p = fmap (\x' -> p { positionX = x' }) $ f (positionX p)
-- xLens (\x -> Just (x+1)) (Position 3 4)
-- Just (Position {positionX = 4.0, positionY = 4.0})
-- xLens (\x -> Nothing) (Position 3 4)
-- Nothing

-- xLens (Double -> Maybe Double)
-- and we get
-- Position -> Maybe Position

-- xLens (\x -> [x+1,x+2,x+3]) (Position 3 4)
-- [Position {positionX = 4.0, positionY = 4.0},Position {positionX = 5.0, positionY = 4.0},Position {positionX = 6.0, positionY = 4.0}]

-- xLens (Double -> [Double])
-- and we get
-- Position -> [Position]


-- view, set, over

xLens' :: Functor f => (Double -> f Double) -> Position -> f Position
xLens' f p = fmap setter $ f $ getter p
  where
    setter :: Double -> Position
    setter x' = p { positionX = x' }
    getter :: Position -> Double
    getter = positionX

-- over :: Lens b a -> (a -> a) -> b -> b
-- over :: Functor f => ((a -> f a) -> b -> f b) -> (a -> a) -> b -> b
-- over' :: Lens Position Double -> (Double -> Double) -> Position -> Position
over :: ((a -> Identity a) -> b -> Identity b) -> (a -> a) -> b -> b
over lens f x = runIdentity $ lens (Identity . f) x
-- over xLens' (+1) $ Position 3 4
-- Position {positionX = 4.0, positionY = 4.0}

-- set :: Lens b a -> a -> b -> b
-- set :: Lens Position Double -> Double -> Position -> Position
-- set xLens give us a function that accepts Double and Position and return Position
set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set lens a' x = over lens (\_ -> a') x
-- set xLens' 5 (Position 3 4)
-- Position {positionX = 5.0, positionY = 4.0}

-- view :: Lens b a -> b -> a
-- view :: Lens Position Double -> Position -> Double
-- view xLens give us a function that accepts Position and return Double
view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view lens x = getConst $ lens Const $ x
-- view xLens' (Position 3 4)
-- 3.0

yLens :: Lens Position Double
yLens f p = fmap (\y' -> p { positionY = y' }) $ f (positionY p)

data Line = Line { lineStart :: Position, lineEnd :: Position } deriving (Show)

startLens :: Lens Line Position
startLens f l = fmap (\s' -> l { lineStart = s' }) $ f (lineStart l)
endLens :: Lens Line Position
endLens f l = fmap (\s' -> l { lineEnd = s' }) $ f (lineEnd l)

-- yLens :: Functor f => (Double -> f Double) -> Position -> f Position
-- endLens :: Functor f => (Position -> f Position) -> Line -> f Line
-- so we can do
-- endLens . yLens :: Functor f => (Double -> f Double) -> Line -> f Line
-- endLens . yLens :: Lens Line Double

line1 :: Line
line1 = Line (Position 0 0) (Position 3 4)
-- set (endLens . yLens) 5 line1
-- Line {lineStart = Position {positionX = 0.0, positionY = 0.0}, lineEnd = Position {positionX = 3.0, positionY = 5.0}}
-- view (endLens . yLens) line1
-- 4.0