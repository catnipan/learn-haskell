module MyUtils.RomanInteger (
  RI(..),
  rIsToInt,
  allRIs,
  smallBigRIPair,
  RIs(..),
) where

import Data.Function(on)

data RI = I | V | X | L | C | D | M deriving (Show, Read, Eq, Ord)

smallBigRIPair :: [(RI, RI)]
smallBigRIPair = [(I,V),(I,X),(X,L),(X,C),(C,D),(C,M)]

allRIs :: [RIs]
allRIs = map Single [I,V,X,L,C,D,M] ++ map (\(ri1,ri2) -> Pair ri1 ri2) smallBigRIPair

rIToInt :: RI -> Int
rIToInt I = 1
rIToInt V = 5
rIToInt X = 10
rIToInt L = 50
rIToInt C = 100
rIToInt D = 500
rIToInt M = 1000

rIsToInt :: RIs -> Int
rIsToInt (Single ri) = rIToInt ri
rIsToInt (Pair ri1 ri2) = rIToInt ri2 - rIToInt ri1

data RIs = Single RI | Pair RI RI deriving (Eq)

instance Show RIs where
  show (Single ri) = show ri
  show (Pair ri1 ri2) = show ri1 ++ show ri2

instance Ord RIs where
  compare = compare `on` getLast
    where getLast :: RIs -> RI
          getLast (Single ri) = ri
          getLast (Pair _ ri) = ri