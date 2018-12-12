import Control.Monad.State
import Control.Monad.Identity
import Data.Foldable(foldlM)
import Data.Function(on)
import Data.Char(isDigit)

type Error = String
type EitherError = Either Error

data Optr = EOF | L_P | R_P | ADD | SUB | DIV | MUL | POW | FACT deriving (Show, Eq, Ord)

cP :: Optr -> Optr -> Either Error Ordering
cP EOF R_P = Left $ "syntax error: ')'"
cP EOF EOF = Right EQ
cP EOF _ = Right LT
cP R_P _ = Left $ "syntax error: ')'"
cP L_P EOF = Left $ "syntax error: unclosed parenthese"
cP L_P R_P = Right EQ
cP FACT L_P = Left $ "syntax error: '!('"
cP L_P _ = Right LT
cP op1 op2 =
  case compare op1 op2 of
    GT -> Right GT
    _ -> Right LT

parseOptr :: Char -> Either Error Optr
parseOptr '+' = Right ADD
parseOptr '-' = Right SUB
parseOptr '*' = Right MUL
parseOptr '/' = Right DIV
parseOptr '^' = Right POW
parseOptr '!' = Right FACT
parseOptr '(' = Right L_P
parseOptr ')' = Right R_P
parseOptr '\0' = Right EOF
parseOptr op = Left $ "not valid character: " ++ [op]

data StackState = Ss { getNumStack :: [Int], getOpStack :: [Optr], readingNum :: Bool } deriving (Show)

splitNumStack :: [Int] -> Bool -> (Int, [Int])
splitNumStack [] False = (0, [])
splitNumStack xxs@(x:xs) False = (0, xxs)
splitNumStack (x:xs) True = (x, xs)

evaluate :: String -> Either Error Int
evaluate str = do
  result <- foldlM updateStackState (Ss [] [EOF] False) (str++"\0")
  return $ head . getNumStack $ result
  where
    updateStackState :: StackState -> Char -> Either Error StackState
    updateStackState st@(Ss nums ops readingNum) chr
      | isDigit chr =
          let n = read [chr] :: Int
              ~(x, xs) = splitNumStack nums readingNum
              newNums = (x*10 + n):xs
          in return $ Ss newNums ops True
      | otherwise = (parseOptr chr) >>= (handle nums ops)
          where
            handle nums [] newOp = return $ Ss nums [] False
            handle nums ops@(topOp:sOps) newOp = do
              order <- cP topOp newOp
              case order of
                LT -> return $ Ss nums (newOp:ops) False
                EQ -> return $ Ss nums ops False
                GT -> (eval nums topOp) >>= (\newNums -> handle newNums sOps newOp)
            eval (x:y:xs) ADD = return $ (x+y):xs
            eval (x:y:xs) SUB = return $ (y-x):xs
            eval (x:y:xs) MUL = return $ (x*y):xs
            eval (x:y:xs) DIV = return $ (y `div` x):xs
            eval (x:y:xs) POW = return $ (y^x):xs
            eval (x:xs) FACT = return $ (product [1..x]):xs
            eval xs EOF = return $ xs
            eval ns op = Left $ "unexpected error: " ++ show ns ++ show op

test :: IO ()
test = mapM_ testCase
  [("1+2", 3),
   ("1+2*4", 9),
   ("12+23*44", 1024),
   ("12+3-4!+2^3*44", 343),
   ("1+2*(3+4)", 15),
   ("1+2*(3+4)*(5+6*(7+8))", 1331),
   ("12+(3-4!+2)^3*44", -301784),
   ("2^(3+4!)", 134217728)]

testCase :: (String, Int) -> IO ()
testCase (expression, expectedResult) =
  case evaluate expression of
    Left err -> putStrLn $ "✗\t" ++ expression ++ ": failed with " ++ err
    Right result ->
      if result == expectedResult
        then putStrLn $ "✓\t" ++ expression ++ " = " ++ show expectedResult
        else putStrLn $ "✗\t" ++ expression ++ ": expected to be " ++ show expectedResult ++ " but get " ++ show result