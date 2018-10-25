-- Given an array of integers, return indices of the two numbers such that they add up to a specific target.

-- You may assume that each input would have exactly one solution, and you may not use the same element twice.

-- Example:

-- Given nums = [2, 7, 11, 15], target = 9,

-- Because nums[0] + nums[1] = 2 + 7 = 9,
-- return [0, 1].

import qualified Data.Map as Map
import Control.Applicative((<|>))

type Indice = Int
type NumIndiceMap = Map.Map Int Indice

data CalcState = Pending NumIndiceMap [Indice] | Solved (Indice,Indice)

twoSum :: [Int] -> Int -> Maybe (Indice, Indice)
twoSum xs target =
  case toIndiceMap idxXss (Pending Map.empty []) of
    Solved (i,j) -> Just (i,j)
    Pending idxMap _ -> foldr (<|>) Nothing $ map (toMaybeIndice idxMap) idxXss
  where
    idxXss = zip [0..] xs
    isHalfTarget :: Int -> Bool
    isHalfTarget = if even target then (==(target `quot` 2)) else const False    
    toIndiceMap :: [(Indice,Int)] -> CalcState -> CalcState
    toIndiceMap [] s = s
    toIndiceMap _ s@(Solved _) = s
    toIndiceMap ((idx,val):rest) (Pending map hs) =
      if isHalfTarget val
        then case hs of
              [] -> toIndiceMap rest (Pending map [idx])
              idx':[] -> (Solved (idx,idx'))
        else toIndiceMap rest (Pending (Map.insert val idx map) hs)
    toMaybeIndice :: NumIndiceMap -> (Indice, Int) -> Maybe (Indice, Indice)
    toMaybeIndice idxMap (i, v) =
      case Map.lookup (target - v) idxMap of
        Just j -> Just (i, j)
        _ -> Nothing

-- twoSum [2, 7, 11, 15] 9
-- Just (0,1)