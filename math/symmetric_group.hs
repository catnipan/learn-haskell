import Data.List

type Permutation = [Int]
type PForm = [Int]

-- getPFormOnNLetters :: Int -> [PForm]
-- getPFormOnNLetters n = map calcPermutationForm $ permutations [1..n]
--   where
calcPermutationForm :: Permutation -> Permutation -> PForm
calcPermutationForm pl1 pl2 = filter (\(p1,p2) -> p1 /= p2) . zip $ pl1 pl2
-- pl1 = [1,2,3,4]
-- pl2 = [2,3,1,4]
-- res = [2,3,1]