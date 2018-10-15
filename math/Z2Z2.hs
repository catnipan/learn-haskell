import Data.List(nub)

data Z2Z2 = E | A | B | C deriving (Show, Eq)

-- E (0,0)
-- A (1,0)
-- B (0,1)
-- C (1,1)

instance Semigroup Z2Z2 where
  E <> a = a
  a <> E = a
  A <> A = E
  A <> B = C
  A <> C = B
  B <> A = A <> B
  B <> B = E
  B <> C = A
  C <> C = E
  C <> A = A <> C
  C <> B = B <> C

inverse :: Z2Z2 -> Z2Z2
inverse E = E
inverse A = A
inverse B = B
inverse C = C

z2z2Group :: [Z2Z2]
z2z2Group = [E,A,B,C]

commutatorSubGroup :: [Z2Z2]
commutatorSubGroup = nub [(x <> y <> inverse x <> inverse y) | x <- z2z2Group, y <- z2z2Group]