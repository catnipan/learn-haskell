import System.Random
import Control.Monad(when)

-- random :: (RandomGen g, Random a) => g -> (a, g)
-- 其中 Random typeclass 是指可以表示装随机值的类型
-- RandomGen typeclass 包括那些可以当作随机源的类型
-- instance RandomGen StdGen where ...
-- StdGen type 是 RandomGen typeclass 的一个 instance

-- mkStdGen :: Int -> StdGen
-- random (mkStdGen 100) :: (Float, StdGen)
-- (0.6512469,651872571 1655838864)
-- random (mkStdGen 100) :: (Bool, StdGen)
-- (True,4041414 40692)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- take 5 $ randoms' (mkStdGen 11) :: [Bool]

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n-1) newGen
  in (value:restOfList, finalGen)

-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
-- 在一个范围内的随机数
-- randomR (1,6) (mkStdGen 34)
-- (6,1400490 40692)

-- randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]
-- 一连串在范围内的随机数
-- take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
-- "xnuhlfwywq"

-- getStdGen :: IO StdGen
-- 一个 IO action，返回类型为随机源 StdGen 类型

main0 = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a','z') gen)

-- newStdGen :: IO StdGen

main1 = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen

main2 = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    newStdGen
    main2