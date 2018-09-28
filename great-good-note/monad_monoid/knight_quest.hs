import Control.Monad

type KnightPos = (Int, Int)

isOnBoard :: KnightPos -> Bool
isOnBoard (c,r) = isValid c && isValid r
  where isValid = flip elem [1..8]

guard' :: Bool -> [()]
guard' True = [()]
guard' False = []

dcdr1 = [(dc, dr) | dc <- [2,-2], dr <- [1,-1]]
dcdr2 = [(dc, dr) | dc <- [1,-1], dr <- [2,-2]]
dcdr = dcdr1 ++ dcdr2

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+dc,r+dr) | (dc,dr) <- dcdr]
  guard' $ isOnBoard (c',r')
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

g :: (Monad m, Num c) => c -> m c
g = (\x -> return (x+1)) <=< (\x -> return (x*100))
-- Just 4 >>= g
-- Just 401

f :: (Num b) => b -> b
f = foldr (.) id [(+1),(*100),(+1)]
-- f 1
-- 201

inNStep :: Int -> KnightPos -> [KnightPos]
inNStep n start = return start >>= moveKnightNSteps
  where moveKnightNSteps = foldr (<=<) return (replicate n moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn n start end = end `elem` inNStep n start