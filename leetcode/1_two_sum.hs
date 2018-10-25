-- Given an array of integers, return indices of the two numbers such that they add up to a specific target.

-- You may assume that each input would have exactly one solution, and you may not use the same element twice.

-- Example:

-- Given nums = [2, 7, 11, 15], target = 9,

-- Because nums[0] + nums[1] = 2 + 7 = 9,
-- return [0, 1].

import qualified Data.Map as Map
import Control.Monad.Writer(Writer, writer, runWriter, tell)
import Control.Applicative((<|>))

type Indice = Int
type NumIndiceMap = Map.Map Int Indice

isHalfOf :: Int -> Int -> Bool
x `isHalfOf` y
  | even y = y `quot` 2 == x
  | otherwise = False

type FoldWriter = Writer [Int] NumIndiceMap

twoSum :: [Int] -> Int -> Maybe (Indice, Indice)
twoSum xs target =
  foldr (<|>) (getHalfAnswer halfIdxs) $ map toMaybeIndice idxXss
  where
    idxXss = zip [0..] xs
    (numIndiceMap, halfIdxs) =
      runWriter $ foldr addToMap (writer (Map.empty, [])) idxXss
    getHalfAnswer (i:j:_) = Just (i,j)
    getHalfAnswer _ = Nothing
    addToMap :: (Indice, Int) -> FoldWriter -> FoldWriter
    addToMap (idx, int) ws = do
      oldMap <- ws
      if int `isHalfOf` target
        then do { tell [idx]; return oldMap }
        else return $ Map.insert int idx oldMap
    toMaybeIndice :: (Indice, Int) -> Maybe (Indice, Indice)
    toMaybeIndice (i, v) =
      case Map.lookup (target - v) numIndiceMap of
        Just j -> Just (i, j)
        _ -> Nothing

-- twoSum [2, 7, 11, 15] 9
-- Just (0,1)