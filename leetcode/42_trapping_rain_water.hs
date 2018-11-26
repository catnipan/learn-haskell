-- Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining.


-- The above elevation map is represented by array [0,1,0,2,1,0,1,3,2,1,2,1]. In this case, 6 units of rain water (blue section) are being trapped. Thanks Marcos for contributing this image!

-- Example:

-- Input: [0,1,0,2,1,0,1,3,2,1,2,1]
-- Output: 6

import Data.List(sortOn)
import Data.Function(on)

type Height = Int
type Index = Int
type WaterSize = Int

headOrZero :: [Int] -> Int
headOrZero [] = 0
headOrZero (x:xs) = x

trap :: [Height] -> WaterSize
trap xs = sum $ map getWaterSize hiPairs
  where
    getWaterSize :: (Height, Index) -> WaterSize
    getWaterSize (currHeight, currIdx) =
      let getLargest comp =
            headOrZero . map ((max 0) . (subtract currHeight) . fst) . filter (comp currIdx . snd) $ sortedHeightIdx
      in (min `on` getLargest) (<) (>)
    hiPairs :: [(Height, Index)]
    hiPairs = zip xs [0..]
    sortedHeightIdx :: [(Height, Index)]
    sortedHeightIdx = reverse . sortOn fst $ hiPairs