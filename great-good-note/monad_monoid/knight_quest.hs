type KnightPos = (Int, Int)

isOnBoard :: KnightPos -> Bool
isOnBoard (c,r) = isValid c && isValid r
  where isValid = flip elem [1..8]

guard :: Bool -> [()]
guard True = [()]
guard False = []

dcdr1 = [(dc, dr) | dc <- [2,-2], dr <- [1,-1]]
dcdr2 = [(dc, dr) | dc <- [1,-1], dr <- [2,-2]]
dcdr = dcdr1 ++ dcdr2

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+dc,r+dr) | (dc,dr) <- dcdr]
  guard $ isOnBoard (c',r')
  return (c', r')
  where 

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = filter isOnBoard [(c+dc,r+dr) | (dc,dr) <- dcdr]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start