-- Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining.


-- The above elevation map is represented by array [0,1,0,2,1,0,1,3,2,1,2,1]. In this case, 6 units of rain water (blue section) are being trapped. Thanks Marcos for contributing this image!

-- Example:

-- Input: [0,1,0,2,1,0,1,3,2,1,2,1]
-- Output: 6

type Height = Int
type WaterSize = Int

trap :: [Height] -> WaterSize
trap xs = sum $ map getTrap $ zip3 (accFromLeft xs) (accFromRight xs) xs
  where
    accFromLeft :: [Height] -> [Height]
    accFromLeft (x:xs) = reverse $ foldl accMax [x] xs
    accFromRight :: [Height] -> [Height]
    accFromRight xs = foldl accMax [x] $ xs'
      where (x:xs') = reverse xs
    accMax :: [Height] -> Height -> [Height]
    accMax mx c = (max (head mx) c):mx
    getTrap :: (Height, Height, Height) -> Height
    getTrap (l, r, h) = (min l r) - h