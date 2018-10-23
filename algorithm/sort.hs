{-# LANGUAGE ScopedTypeVariables #-}
import Data.List ((\\))

bubbleSort :: forall a. (Ord a) => [a] -> [a]
bubbleSort xs =
  case bubbleXs xs of
    [] -> []
    (s:rst) -> s:(bubbleSort rst)
  where
    bubbleXs :: [a] -> [a]
    bubbleXs xs = foldr bubble [] xs
    bubble :: a -> [a] -> [a]
    bubble x [] = [x]
    bubble x' xss@(x:xs)
      | x < x' = x:x':xs
      | otherwise = x':xss

selectionSort :: (Ord a) => [a] -> [a]
selectionSort xs = let (_, maxList) = select (xs, []) in maxList
  where
    select :: (Ord a) => ([a],[a]) -> ([a],[a])
    select result@([],maxList) = result
    select (xs, maxList) =
      let newMax = maximum xs
      in select (xs \\ [newMax], newMax:maxList)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeTwo (mergeSort fxs) (mergeSort sxs)
  where 
    middleIdx = length xs `quot` 2
    (fxs, sxs) = splitAt middleIdx xs
    mergeTwo :: (Ord a) => [a] -> [a] -> [a]
    mergeTwo xs ys = reverse $ merge xs ys []
    merge :: (Ord a) => [a] -> [a] -> [a] -> [a]
    merge [] [] ms = ms
    merge [] (y:ys) ms = merge [] ys (y:ms)
    merge (x:xs) [] ms = merge xs [] (x:ms)
    merge (x:xs) (y:ys) ms =
      if x < y then merge xs (y:ys) (x:ms)
                else merge (x:xs) ys (y:ms)
