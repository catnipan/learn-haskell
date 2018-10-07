-- import Algebra.Additive ??
-- TODO define Zm, ZmPair as a ring
import Data.List

type Rank = Int
data Zm = Zm Rank Int deriving (Eq)

instance Show Zm where
  show zm = show . getValInZm $ zm

getValInZm :: Zm -> Int
getValInZm (Zm rank val) = (val `mod` rank)

instance Num Zm where
  (Zm r x) + (Zm r' y) = if r == r' then Zm r $ (x + y) `mod` r else undefined
  (Zm r x) * (Zm r' y) = if r == r' then Zm r $ (x * y) `mod` r else undefined
  negate (Zm r x) = Zm r (-x)
  abs _ = undefined
  signum _ = undefined
  fromInteger _ = undefined

newtype ZmPair = ZmPair { getZmPair :: (Zm, Zm) } deriving (Eq)

instance Num ZmPair where
  ZmPair (m,n) + ZmPair (m',n') = ZmPair (m+m',n+n')
  ZmPair (m,n) * ZmPair (m',n') = ZmPair (m*m',n*n')
  negate (ZmPair (m,n)) = ZmPair (negate m, negate n)
  abs _ = undefined
  signum _ = undefined
  fromInteger _ = undefined

getZmPairRank :: ZmPair -> Rank
getZmPairRank zmPair = (+1) . length . takeWhile (/=zmPair) . tail . iterate (+zmPair) $ zmPair
-- getZmPairRank $ ZmPair(Zm 3 3,Zm 4 1)
-- note：未检测ZmPair 中有一个是零元

instance Show ZmPair where
  show (ZmPair (zm,zm')) = "(" ++ show zm ++ "," ++ show zm' ++ ")"

makeZmRing :: Rank -> [Zm]
makeZmRing m = map (Zm m) [0..m-1]

makeZmPairRing :: (Rank, Rank) -> [ZmPair]
makeZmPairRing (r,r') = map ZmPair $ zip zrs zr's
  where zrs = mconcat . replicate r' . makeZmRing $ r
        zr's = mconcat . replicate r . makeZmRing $ r'

printZmPairRingDoubleLine :: [ZmPair] -> IO ()
printZmPairRingDoubleLine zmPairs = 
  mapM_ (putStrLn . showZms) [fstZms, sndZms]
  where
    (fstZms, sndZms) = unzip . map getZmPair $ zmPairs
    showZms zms = intercalate "\t" . map show $ zms

-- printZmPairRingDoubleLine $ makeZmPairRing (3,4)
-- 0       1       2       0       1       2       0       1       2       0       1       2
-- 0       1       2       3       0       1       2       3       0       1       2       3

printTianGanDizhi :: IO ()
printTianGanDizhi = putStrLn . intercalate " " . map tgdzPairToString . take 60 . makeZmPairRing $ (10,12)
  where
    tgdzPairToString (ZmPair (tg, dz)) = (showTianGan tg ++ showDiZhi dz)
    showTianGan :: Zm -> String
    showTianGan zm@(Zm 10 i) = tianGan !! (getValInZm zm)
      where tianGan = ["甲","乙","丙","丁","戊","己","庚","辛","壬","癸"]
    showTianGan _ = undefined
    showDiZhi :: Zm -> String
    showDiZhi zm@(Zm 12 i) = dizhi !! (getValInZm zm)
      where dizhi = ["子","丑","寅","卯","辰","巳","午","未","申","酉","戌","亥"]
    showDiZhi _ = undefined


-- 今有物不知其数,三三数之剩二;五五数之剩三,七七数之剩二;问物几何?
type Mod = Int
type Remainder = Int
-- calcNumFromModRemainder [(3,2),(5,3),(7,2)] = 23
-- in which Mod is prime with each other
-- and 0 <= remainder < mod

calcNumFromModRemainder :: [(Mod, Remainder)] -> Int
calcNumFromModRemainder ((fstMod, fstReminader):mrs) =
  calcNum mrs fstMod fstReminader
  where
    calcNum :: [(Mod, Remainder)] -> Int -> Int -> Int
    calcNum [] _ currNum = currNum
    calcNum ((newMod, newRemainder):xs) currGap currNum =
      let newNum = head $ dropWhile ((/=newRemainder) . (`mod` newMod)) $ iterate (+currGap) currNum
          newGap = currGap * newMod
      in calcNum xs newGap newNum