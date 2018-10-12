import Data.Function (on)
import Data.List (nub)

type Rank = Int

data Z = Z Rank Int

instance Semigroup Z where
  (Z r1 v1) <> (Z r2 v2) = if r1 /= r2 then undefined else Z r1 (v1 + v2)

instance Eq Z where
  (Z r1 v1) == (Z r2 v2) = if r1 /= r2
                            then undefined
                            else ((==) `on` (`mod` r1)) v1 v2

instance Show Z where
  show (Z r v) = (++ "(" ++ show r ++ ")") . show . (`mod` r) $ v

z12Group :: [Z]
z12Group = map (Z 12) [0..11]

z18Group :: [Z]
z18Group = map (Z 18) [0..17]

type ZZMapping = Z -> Z

times_ :: Int -> ZZMapping
times_ x (Z r v)= (Z 18) . (* x) . (`mod` r) $ v

type ErrorCase = String

listZ12Mapping :: ZZMapping -> [String]
listZ12Mapping zzmp = map getMapInfoStr z12Group
  where getMapInfoStr z12 = show z12 ++ " -> " ++ show (zzmp z12)

testZ12Homomorphism :: ZZMapping -> [ErrorCase]
testZ12Homomorphism zzmp = allCases >>= testCase
  where
    allCases = (,) <$> z12Group <*> z12Group
    testCase (z1@(Z 12 _), z2@(Z 12 _)) = 
      if ma /= mb then [ "x\t" ++ maStr ++ "\t" ++ mbStr ] else []
      where ma = zzmp (z1 <> z2)
            maStr = "φ(" ++ show z1 ++ "+" ++ show z2 ++ ") = " ++ show ma
            mb = (zzmp z1) <> (zzmp z2)
            mbStr = "φ(" ++ show z1 ++ ")+φ(" ++ show z2 ++ ") = " ++ show mb

-- mapM putStrLn $ testZ12Homomorphism (times_ 2)
-- mapM putStrLn $ testZ12Homomorphism (times_ 3)
-- x_ is ZZMapping creator from Z12Group to Z18Group
-- when take 0,9,6,12,3,15 it is homomorphism

-- calculate (times_ ?) homomorphism from (Zn, +) to (Zm, +)
-- 1(from Zn) * n = 0 (from Zn)
-- φ(1)(from Zm) * n = 0 (from Zm) (because of homomorphism)
-- so n is divisible by the rank of φ(1)
-- whereas |φ(1)| = m / (m, φ(1))
calcHomomorphismFromZnToZm :: Rank -> Rank -> [Int]
calcHomomorphismFromZnToZm r1 r2 = filter isGood [0..(r2-1)]
  where isGood x = (r2 `quot` (gcd r2 x)) `isZC` r1
        x `isZC` y = y `mod` x == 0