module MyUtils.ToUniqTriple (
  toUniqTriple,
) where

toUniqTriple :: [a] -> [(a,a,a)]
toUniqTriple xs = do
  (aIdx,a) <- idxXs
  (bIdx,b) <- drop aIdx idxXs
  (_,c) <- drop bIdx idxXs
  return (a,b,c)
  where idxXs = zip [1..] xs