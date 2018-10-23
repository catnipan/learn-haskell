-- Merge k sorted linked lists and return it as one sorted list. Analyze and describe its complexity.

-- Example:

-- Input:
-- [
--   1->4->5,
--   1->3->4,
--   2->6
-- ]
-- Output: 1->1->2->3->4->4->5->6

{-# LANGUAGE ScopedTypeVariables #-}

mergeKLists :: forall a. (Ord a) => [[a]] -> [a]
mergeKLists [] = []
mergeKLists [[]] = []
mergeKLists xss = let ~((sx:srxs):rxss) = bubbleXss xss in sx:(mergeKLists (srxs:rxss))
  where
    bubbleXss :: [[a]] -> [[a]]
    bubbleXss xss = foldr bubble [] xss
    bubble :: [a] -> [[a]] -> [[a]]
    bubble [] nxss = nxss
    bubble xs' [] = [xs']
    bubble xs'@(x':_) nxss@(xs@(x:_):rst)
      | x < x' = xs:xs':rst
      | otherwise = xs':nxss