data Position = Position { positionX :: Double, positionY :: Double } deriving (Show)

p = Position 1 2

p2 = p { positionY = 3 }
-- is the same as
p3 = Position {
  positionX = positionX p,
  positionY = 3
}

-- because all the bindings (name -> underlying data) will not change,
-- p, p2, p3 share the same memory of 'positionX'

positionX' :: Position -> Double
positionX' (Position x _) = x

setPositionX :: Double -> Position -> Position
setPositionX x' p =  p { positionX = x' }

setPositionY :: Double -> Position -> Position
setPositionY y' p =  p { positionY = y' }

-- when the data structure become more complex
data Line = Line { lineStart :: Position, lineEnd :: Position } deriving (Show)

line1 = Line (Position 0 0) (Position 3 4)
-- to change the y-coordinate of lineEnd of line1, to 5

-- pattern match style
line2 = case line1 of Line p1 (Position x _) -> Line p1 (Position x 5)

-- record syntax style
line2' = line1 { lineEnd = (lineEnd line1) { positionY = 5} }

-- setter
setLineEnd :: Position -> Line -> Line
setLineEnd p l = l { lineEnd = p }

line2'' = setLineEnd ( setPositionY 5 (lineEnd line1) ) line1