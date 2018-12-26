import qualified Data.Set as Set
type Size = Int

newtype Queen = Queen (Int,Int) deriving (Eq, Ord, Show)
data Solution = Solution Size [Queen]

instance Show Solution where
  show (Solution size qs) = 
    range >>= (\x -> (
        (range >>= (\y -> 
          if ((Queen (x,y)) `Set.member` qset)
            then "x"
            else "."
        )) ++ "\n"
      ))
    where
      qset = Set.fromList qs
      range = [0..size-1]

isThreaten :: Queen -> Queen -> Bool
(Queen (x,y)) `isThreaten` (Queen (x', y')) =
  (x == x') || (y == y') || ((x + y) == (x' + y')) || ((y - x) == (y' - x'))

queenPuzzle :: Size -> [Solution]
queenPuzzle size = solve [Queen (0,0)]
  where
    maxIdx = size - 1
    solve :: [Queen] -> [Solution]
    solve [] = []
    solve qs@((Queen (x,y)):rqs)
      | y == maxIdx = (Solution size qs):(solve (next rqs))
      | otherwise = solve tryMore
      where
        tryMore :: [Queen]
        tryMore =
          case findFirstNonThreatenWith qs . map (\x -> Queen (x,y+1)) $ [0..maxIdx] of
            Just nq -> nq:qs
            Nothing -> next qs
    next :: [Queen] -> [Queen]
    next [] = []
    next ((Queen (x,y)):rqs) =
      case findFirstNonThreatenWith rqs . map (\x -> Queen (x,y)) $ [x+1..maxIdx] of
        Just q -> q:rqs
        Nothing -> next rqs
    findFirstNonThreatenWith :: [Queen] -> [Queen] -> Maybe Queen
    findFirstNonThreatenWith _ [] = Nothing
    findFirstNonThreatenWith qs (q:rqs) =
      if any (isThreaten q) qs
        then findFirstNonThreatenWith qs rqs
        else Just q

