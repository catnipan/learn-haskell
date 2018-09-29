isPrimeWith :: Int -> Int -> Bool
n `isPrimeWith` p = gcd n p == 1

calcZmInverses :: Int -> [Int]
calcZmInverses m = filter (`isPrimeWith` m) [1..m]

φm :: Int -> Int
φm m = length . calcZmInverses $ m

showInfoφm :: Int -> String
showInfoφm m = ("φ(" ++ (show m) ++ ")=" ++ (show $ φm m) ++ "\t" ++ (show . calcZmInverses $ m))

showφmList :: [Int] -> IO ()
showφmList ms = mapM_ putStrLn . map showInfoφm $ ms

type Rank = Int
data Zm = Zm { getRank :: Rank, _value :: Int }

instance Show Zm where
  show zm = show . getValue $ zm

getValue :: Zm -> Int
getValue (Zm rank val) = val `mod` rank
