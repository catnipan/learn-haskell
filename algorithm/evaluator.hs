import Control.Monad.State
import Control.Monad.Identity
import Data.List(groupBy)
import Data.Foldable(foldlM)
import Data.Function(on)
import Data.Char(isDigit)
import Data.List(intercalate)
import Control.Monad.Writer (Writer, tell, runWriterT, execWriterT)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.Trans.State.Lazy (StateT)

data CalcState =
  CalcState { nums :: [Int], ops :: [Optr] } deriving (Show)

type ErrString = String
type EvalString = String
data EvalItem = Operator Optr | Number Int deriving Show
data Optr = EOF | L_P | R_P | ADD | SUB | DIV | MUL | POW | FACT deriving (Show, Eq, Ord)
type RPN = [EvalItem]
type EvalMonad = StateT RPN (ExceptT ErrString (Writer [EvalString]))

-- when reading an operator
-- 1. push to stack
-- 2. pop stack op, calculate... loop until we can push
-- 3. cancel stack top op out
data OpHandle = PUSH | POP_AND_CALC | CANCEL_OUT

handleOp :: Optr -> Optr -> Either ErrString OpHandle
handleOp topOp newOp =
  case (topOp, newOp) of
    (EOF, R_P) -> Left $ "syntax error: ')'"
    (EOF, EOF) -> Right CANCEL_OUT
    (EOF, _) -> Right PUSH
    (R_P, _) -> Left $ "syntax error: ')'"
    (L_P, EOF) -> Left $ "syntax error: unclosed parenthese"
    (L_P, R_P) -> Right CANCEL_OUT
    (FACT, L_P) -> Left $ "syntax error: '!('"
    (_, L_P) -> Right PUSH
    _ -> case compare topOp newOp of
          GT -> Right POP_AND_CALC
          _ -> Right PUSH

parseOp :: Char -> Either ErrString Optr
parseOp opChar =
  case opChar of
    '+' -> Right ADD
    '-' -> Right SUB
    '*' -> Right MUL
    '/' -> Right DIV
    '^' -> Right POW
    '!' -> Right FACT
    '(' -> Right L_P
    ')' -> Right R_P
    '\0' -> Right EOF
    _ -> Left $ "not valid character: " ++ [opChar]

parse :: [String] -> [Either ErrString EvalItem]
parse strs = do
  items <- strs
  if isDigit $ head items
    then [Right . Number . toNum $ items]
    else map toOptr items
  where
    toNum ds = foldl (\result d -> 
        let n = read [d] :: Int
        in result * 10 + n
      ) 0 ds
    toOptr chr = do
      op <- parseOp chr
      return $ Operator op

parseExp :: String -> [Either ErrString EvalItem]
parseExp str = parse . groupBy ((==) `on` isDigit) $ str

parseRPN :: String -> [Either ErrString EvalItem]
parseRPN str = parse . words $ str

tellAndThrowError :: ErrString -> EvalMonad a
tellAndThrowError err = do { tell [err]; throwError err; }

record :: (Show a) => String -> a -> EvalMonad ()
record tag value = tell [tag ++ ": " ++ show value]

showState :: CalcState -> EvalMonad ()
showState cs = tell ["~ " ++ show cs]

appendRPN :: EvalItem -> EvalMonad ()
appendRPN evalItem = modify ((:) evalItem)

evaluate :: String -> EvalMonad CalcState
evaluate str =
  foldlM calc' (CalcState [] [EOF]) (parseExp str ++ [Right (Operator EOF)])
  where
    calc' :: CalcState -> Either ErrString EvalItem -> EvalMonad CalcState
    calc' cs item = do { showState cs; calc cs item; }
    calc :: CalcState -> Either ErrString EvalItem -> EvalMonad CalcState
    calc _ (Left err) = tellAndThrowError err
    calc (CalcState nums ops) (Right (Number n)) = do
      record "push num" n
      appendRPN (Number n)
      return $ CalcState (n:nums) ops
    calc (CalcState nums ops) (Right (Operator op)) = handle nums ops op
      where
        handle :: [Int] -> [Optr] -> Optr -> EvalMonad CalcState
        handle nums ops@(topOp:restOps) newOp =
          case handleOp topOp newOp of
            Left err -> tellAndThrowError err
            Right PUSH -> do
              record "push Op" newOp
              return $ CalcState nums (newOp:ops)
            Right CANCEL_OUT -> do
              record "cancel out Op" newOp
              return $ CalcState nums restOps
            Right POP_AND_CALC -> do
              record "new Op wait" newOp
              record "pop and calculate Op" topOp
              appendRPN (Operator topOp)
              newNum <- eval nums topOp
              handle newNum restOps newOp
        eval :: [Int] -> Optr -> EvalMonad [Int]
        eval ns op =
          case evalOp ns op of
            Right rns -> return rns
            Left err -> throwError err

evalOp :: [Int] -> Optr -> Either ErrString [Int]
evalOp (x:y:xs) ADD = return $ (x+y):xs
evalOp (x:y:xs) SUB = return $ (y-x):xs
evalOp (x:y:xs) MUL = return $ (x*y):xs
evalOp (x:y:xs) DIV = return $ (y `div` x):xs
evalOp (x:y:xs) POW = return $ (y^x):xs
evalOp (x:xs) FACT = return $ (product [1..x]):xs
evalOp ns op = Left $ "unexpected error: " ++ show ns ++ show op

evaluateRPN :: String -> Either ErrString Int
evaluateRPN str =
  fmap head $ foldlM calc [] (parseRPN str)
  where
    calc :: [Int] -> Either ErrString EvalItem -> Either ErrString [Int]
    calc _ (Left e) = Left e
    calc nums (Right (Number n)) = return $ n:nums
    calc nums (Right (Operator op)) = evalOp nums op

getRPN :: EvalMonad CalcState -> Either ErrString String
getRPN ec = do
  (_, rpnStack) <- fst . runIdentity . runWriterT . runExceptT $ runStateT ec []
  return $ intercalate " " . fmap evalItem2str . reverse $ rpnStack
  where
    evalItem2str :: EvalItem -> String
    evalItem2str (Number num) = show num
    evalItem2str (Operator op) =
      case op of
        ADD -> "+"
        SUB -> "-"
        DIV -> "/"
        MUL -> "*"
        POW -> "^"
        FACT -> "!"

-- examine :: EvalMonad CalcState -> IO ()
-- examine = mapM_ putStrLn . runIdentity . execWriterT . runExceptT

-- eval :: String -> Int
-- eval str = 
--   case fst . runIdentity . runWriterT . runExceptT $ evaluate str of
--     Right (CalcState (result:[]) ops) -> result
--     _ -> undefined

-- test :: IO ()
-- test = mapM_ testCase
--   [("1+2", 3),
--    ("1+2*4", 9),
--    ("12+23*44", 1024),
--    ("12+3-4!+2^3*44", 343),
--    ("1+2*(3+4)", 15),
--    ("1+2*(3+4)*(5+6*(7+8))", 1331),
--    ("12+(3-4!+2)^3*44", -301784),
--    ("2^(3+4!)", 134217728)]

-- testCase :: (String, Int) -> IO ()
-- testCase (expression, expectedResult) =
--   case evaluate expression of
--     Left err -> putStrLn $ "✗\t" ++ expression ++ ": failed with " ++ err
--     Right result ->
--       if result == expectedResult
--         then putStrLn $ "✓\t" ++ expression ++ " = " ++ show expectedResult
--         else putStrLn $ "✗\t" ++ expression ++ ": expected to be " ++ show expectedResult ++ " but get " ++ show result