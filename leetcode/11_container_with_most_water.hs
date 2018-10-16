-- Given n non-negative integers a1, a2, ..., an , where each represents a point at coordinate (i, ai). n vertical lines are drawn such that the two endpoints of line i is at (i, ai) and (i, 0). Find two lines, which together with x-axis forms a container, such that the container contains the most water.

-- Note: You may not slant the container and n is at least 2.

-- Example:

-- Input: [1,8,6,2,5,4,8,3,7]
-- Output: 49

toPairWith :: (a -> a -> b) -> [a] -> [b]
toPairWith _ [] = []
toPairWith f (x:xs) = [f x m | m <- xs] ++ (toPairWith f xs)

type Width = Int
type Height = Int
type Area = Int

maxArea :: [Height] -> Area
maxArea hs = maximum . toPairWith getArea . zip [0..] $ hs
  where
    getArea :: (Width, Height) -> (Width, Height) -> Area
    getArea (x1,h1) (x2,h2) = (min h1 h2) * (x2 - x1)