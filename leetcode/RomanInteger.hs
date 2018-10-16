module RomanInteger (
  RI(..),
  rIToInt,
  smallBigRIPair,
  RIs(..),
) where

import Data.Function(on)

data RI = I | V | X | L | C | D | M deriving (Show, Read, Eq, Ord)

smallBigRIPair :: [(RI, RI)]
smallBigRIPair = [(I,V),(I,X),(X,L),(X,C),(C,D),(C,M)]

rIToInt :: RI -> Int
rIToInt I = 1
rIToInt V = 5
rIToInt X = 10
rIToInt L = 50
rIToInt C = 100
rIToInt D = 100
rIToInt M = 1000

data RIs = Single RI | Pair RI RI deriving (Eq)

instance Show RIs where
  show (Single ri) = show ri
  show (Pair ri1 ri2) = show ri1 ++ show ri2

instance Ord RIs where
  compare = compare `on` getLast
    where getLast :: RIs -> RI
          getLast (Single ri) = ri
          getLast (Pair _ ri) = ri