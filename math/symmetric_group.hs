import Data.List

type PermuGroup = [Int]
type SearchList = [(Int, Int)]

newtype PermuItem = PermuItem { getPermuItem :: [PermuGroup] }

showPermuGroup :: PermuGroup -> String
showPermuGroup (_:[]) = ""
showPermuGroup groups = "(" ++ (mconcat . intersperse "," . map show $ groups) ++ ")"

instance Show PermuItem where
  show (PermuItem permuGroups) =
    let str = mconcat . map showPermuGroup $ permuGroups
    in if str == "" then "(1)" else str

startPermuOn :: Int -> SearchList -> (PermuGroup, SearchList)
startPermuOn firstStart searchList = (permuGroup', searchList')
  where 
    calcOnStart :: Int -> PermuGroup -> PermuGroup
    calcOnStart start permuGroup = case lookup start searchList of
      Nothing -> permuGroup
      Just thenStart ->
        if thenStart == firstStart
          then permuGroup
          else calcOnStart thenStart (thenStart:permuGroup)
    searchList' = filter (not . (`elem` permuGroup') . fst) searchList
    permuGroup' = reverse $ calcOnStart firstStart [firstStart]

searchListToPermuGroupList :: SearchList -> PermuItem
searchListToPermuGroupList startSearchList = iterator startSearchList $ PermuItem []
  where
    iterator :: SearchList -> PermuItem -> PermuItem
    iterator searchList@((start,_):_) (PermuItem permuItem) =
      let (newPermuGroup, newSearchList) = startPermuOn start searchList
      in iterator newSearchList (PermuItem (newPermuGroup:permuItem))
    iterator _ permuItem = permuItem

symmetricGroupN :: Int -> [PermuItem]
symmetricGroupN n = map getPermuItemOnPermutation $ permutations [1..n]
  where
    getPermuItemOnPermutation :: [Int] -> PermuItem
    getPermuItemOnPermutation permu = searchListToPermuGroupList $ zip [1..n] permu