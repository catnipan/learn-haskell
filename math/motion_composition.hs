type Time = Float
type XPosition = Float
type YPosition = Float
type XYPosition = (XPosition, YPosition)

type XMotion = Time -> XPosition
type YMotion = Time -> YPosition
type XYMotion = Time -> XYPosition

composeMotion :: XMotion -> YMotion -> XYMotion
composeMotion xMotion yMotion = \time -> (xMotion time, yMotion time)

projectMotionX :: XYMotion -> XMotion
projectMotionX xyMotion = \time -> fst . xyMotion $ time

projectMotionY :: XYMotion -> YMotion
projectMotionY xyMotion = \time -> snd . xyMotion $ time

xM :: XMotion
xM t = 3 * t
yM :: YMotion
yM t = yM t = 0.5 * 10 * t * t

-- composeMotion xM yM 3