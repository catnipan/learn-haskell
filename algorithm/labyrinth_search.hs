import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Trans.State.Lazy(State, gets, get, modify, evalState)

size :: Int
size = 20

type Position = Int
type Path = [Position]

fromRowCol :: (Int, Int) -> Position
fromRowCol (row, col) = row * size + col

walls :: Set.Set Position
walls = Set.fromList $
  [ fromRowCol (row, col) | row <- [0..size - 1], col <- [0, size - 1]] ++
  [ fromRowCol (row, col) | col <- [0..size - 1], row <- [0, size - 1]]

blocks :: Set.Set Position
blocks = Set.fromList $
  [42,62,102,122,142,162,161,63,84,104,124,144,201,202,203,165,185,225,244,243,242,262,226,227,187,207,106,126,147,167,66,45,43,44,28,48,87,108,128,127,107,89,70,50,54,51,53,52,55,57,96,95,93,116,117,137,157,177,197,237,238,235,176,195,215,257,277,276,274,275,253,233,213,193,173,153,155,154,134,133,132,111,130,150,169,170,171,191,211,210,209,229,266,267,268,302,322,362,342,304,344,324,345,346,308,348,368,328,326,285,290,286,310,330,270,271,251,313,312,293,332,372,371,334,315,316,317,337,374,375,376,228,269,331,338]

wallsAndBlocks = Set.union walls blocks
-- during search,
-- we need to record dead positions,
-- and the direction of each position in the path

data Direction = UNKNOWN | RIGHT | DOWN | LEFT | UP | NO_WAY deriving (Enum, Show)

data SearchState = SearchState {
  getPath :: Path,
  getPosDirectionMap :: Map.Map Position Direction,
  getDeadPosSet :: Set.Set Position,
  getPathSet :: Set.Set Position
}

calcNewPos :: Direction -> Position -> Position
calcNewPos UP pos = pos - size
calcNewPos DOWN pos = pos + size
calcNewPos LEFT pos = pos - 1
calcNewPos RIGHT pos = pos + 1

start :: Position
start = fromRowCol (1,1)

end :: Position
end = fromRowCol (size-2,size-2)

labyrinthSearch :: Set.Set Position -> Position -> Position -> Path
labyrinthSearch wallsAndBlocks start end = evalState search initState
  where
    initState :: SearchState
    initState = SearchState {
      getPath = [start],
      getPosDirectionMap = Map.empty,
      getDeadPosSet = Set.empty,
      getPathSet = Set.fromList [start]
    }
    search :: State SearchState Path
    search = do
      path <- gets getPath
      searchFrom path
    searchFrom [] = return []
    searchFrom path@(currPos:beforePath)
      | currPos == end = return path
      | otherwise = do
          nextResult <- findNextToGo currPos
          case nextResult of
            Just (nextD, newPos) ->
              let tryMore (SearchState path pdMap ds pathSet) = SearchState (newPos:path) (Map.insert currPos nextD pdMap) ds (Set.insert newPos pathSet)
              in modify tryMore
              -- add newPos to the path, and record nextD as currPos in map
            Nothing ->
              let backStep (SearchState _ pdMap ds pathSet) = SearchState beforePath pdMap (Set.insert currPos ds) (Set.delete currPos pathSet)
              in modify backStep
              -- goBack, record currPos ad a dead one
          search -- continue to search
    findNextToGo :: Position -> State SearchState (Maybe (Direction, Position))
    findNextToGo currPos = do
      (SearchState _ posDirectionMap deadPosSet pathSet) <- get
      return $ findFirst (\newPos -> any (newPos `Set.member`) [wallsAndBlocks, deadPosSet, pathSet]) [(succ $ lookupIn posDirectionMap)..NO_WAY]
      where
        lookupIn :: Map.Map Position Direction -> Direction
        lookupIn posDirectionMap =
          case Map.lookup currPos posDirectionMap of
            Nothing -> UNKNOWN
            Just d -> d
        findFirst _ (NO_WAY:_) = Nothing
        findFirst isInvalid (nextD:restD) =
          let newPos = calcNewPos nextD currPos
          in if isInvalid newPos
              then findFirst isInvalid restD
              else return (nextD, newPos)

-- labyrinthSearch wallsAndBlocks start end
-- [378,358,357,356,355,354,353,352,351,350,349,329,309,289,288,287,307,327,347,367,366,365,364,363,343,323,303,283,282,281,261,241,221,222,223,224,204,184,164,163,143,123,103,83,82,81,61,41,21]

printLabyrinthSearch :: Set.Set Position -> Position -> Position -> IO ()
printLabyrinthSearch wallsAndBlocks start end = putStrLn searchResult
  where
    path = labyrinthSearch wallsAndBlocks start end
    pathSet = Set.fromList path
    searchResult :: String
    searchResult =[0..size-1] >>= (\row -> ([0..size-1] >>= (\col -> showPos $ fromRowCol (row, col))) ++ "\n")
    showPos :: Position -> String
    showPos idx
      | idx == start = "x"
      | idx == end = "x"
      | idx `Set.member` wallsAndBlocks = "●"
      | idx `Set.member` pathSet = "○"
      | otherwise = "‧"

-- printLabyrinthSearch wallsAndBlocks start end
-- ●●●●●●●●●●●●●●●●●●●●
-- ●x‧‧‧‧‧‧●‧‧‧‧‧‧‧‧‧‧●
-- ●○●●●●‧‧●‧●●●●●●‧●‧●
-- ●○●●‧‧●‧‧‧●‧‧‧‧‧‧‧‧●
-- ●○○○●‧‧●‧●‧‧‧●‧●●‧‧●
-- ●‧●○●‧●●●‧‧●‧‧‧‧●●‧●
-- ●‧●○●‧●●●‧●‧●●●‧‧●‧●
-- ●‧●○●‧‧●‧‧●‧‧●●●‧●‧●
-- ●●●○○●‧●‧●●●‧●‧‧●●‧●
-- ●‧‧‧○●‧●‧‧‧●‧●‧●‧●‧●
-- ●●●●○‧‧●‧●●●‧●‧●‧‧‧●
-- ●○○○○●●●●●‧‧‧●‧●‧●●●
-- ●○●●●‧‧‧‧‧‧●‧●‧‧‧●‧●
-- ●○●‧‧‧●●●●●●‧‧●●●●‧●
-- ●○○○‧●●○○○●‧‧●‧‧‧‧‧●
-- ●‧●○●‧‧○●○●‧●●‧●●●‧●
-- ●‧●○●‧●○●○●●●‧●‧‧●●●
-- ●‧●○●●●○●○○○○○○○○○○●
-- ●‧●○○○○○●‧‧●●‧●●●‧x●
-- ●●●●●●●●●●●●●●●●●●●●