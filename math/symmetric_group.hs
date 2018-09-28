import Data.List

type Pair = (Int,Int)
type PermutationGroup = [Int]

newtype Permutation = Permutation { getPermutation :: [Int] }

instance Semigroup Permutation where
  Permutation pa <> Permutation pb = Permutation . map ((pb !!) . (subtract 1)) $ pa

instance Show Permutation where
  show (Permutation permutation) = if str == "" then "(1)" else str
    where
      str = mconcat . map showPermuGroup . pairListToPermutationGroupList . zip [1..] $ permutation
      showPermuGroup :: PermutationGroup -> String
      showPermuGroup (_:[]) = ""
      showPermuGroup groups = "(" ++ (mconcat . intersperse "," . map show $ groups) ++ ")"

pairListToPermutationGroupList :: [Pair] -> [PermutationGroup]
pairListToPermutationGroupList pairList = iterator pairList []
  where
    iterator :: [Pair] -> [PermutationGroup] -> [PermutationGroup]
    iterator searchList@((start,_):_) permuItem =
      let (newPermuGroup, newSearchList) = startPermuOn start searchList
      in iterator newSearchList (newPermuGroup:permuItem)
    iterator _ permuItem = permuItem
    startPermuOn :: Int -> [Pair] -> (PermutationGroup, [Pair])
    startPermuOn firstStart searchList = (permuGroup', searchList')
      where 
        calcOnStart :: Int -> PermutationGroup -> PermutationGroup
        calcOnStart start permuGroup = case lookup start searchList of
          Nothing -> permuGroup
          Just thenStart ->
            if thenStart == firstStart
              then permuGroup
              else calcOnStart thenStart (thenStart:permuGroup)
        searchList' = filter (not . (`elem` permuGroup') . fst) searchList
        permuGroup' = reverse $ calcOnStart firstStart [firstStart]

makePermutation :: [Int] -> Permutation
makePermutation xs
  | sort xs == [1..length xs] = Permutation xs
  | otherwise = undefined

symmetricGroupN :: Int -> [Permutation]
symmetricGroupN n = map Permutation . permutations $ [1..n]

symmetricGroup3 = symmetricGroupN 3

compositionTable :: (Semigroup m, Show m) => [m] -> IO ()
compositionTable semigroup = mapM_ putStrLn [(showCalc a b) | a <- semigroup, b <- semigroup]
  where showCalc a b = (show a ++ " <> " ++ show b ++ " = " ++ show (a <> b))