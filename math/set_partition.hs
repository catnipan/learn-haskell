{-# LANGUAGE ScopedTypeVariables #-}

import Data.List

newtype SP a = SP { getSP :: [[a]] }

instance (Show a) => Show (SP a) where
  show (SP xs) = "(" ++ (partitionsToSlash $ map partitionToComma xs) ++ ")"
    where
      partitionToComma = intercalate "," . map show -- [1,2,3] -> "1,2,3"
      partitionsToSlash = intercalate "/" -- ["1,2,3", "4"] -> "1,2,3/4"

addElement :: forall a.a -> SP a -> [SP a]
addElement e = add []
  where
    add :: [[a]] -> SP a -> [SP a]
    add xps (SP (p:ps)) =
      let newP = SP ((e:p):ps ++ xps)
      in newP:(add (p:xps) (SP ps))
    add xps (SP []) = [SP ([e]:xps)]

-- enumerate all partition of a set
setPartition :: Int -> [SP Int]
setPartition 0 = [SP []]
setPartition n = (setPartition (n-1)) >>= (addElement n)
-- setPartition 3
-- [(3,2,1),(3/2,1),(3,2/1),(3,1/2),(3/1/2)]