
import Control.Monad.State
import Control.Monad.Identity

-- "1+2+3"
-- "1+(2+3)"
-- "1+2*3"

-- "12+13"

type Error = String
type EitherError = Either Error

data ParenSymbol = L_P | R_P deriving (Show)
data OpSymbol = ADD | MUL deriving (Show)
data Op = BinOp OpSymbol | UnaryOp OpSymbol | Paren ParenSymbol deriving (Show)

data StackState = StackState { getNumStack :: [Int], getOpStack :: [Op] } deriving (Show)

updateNumStack :: ([Int] -> [Int]) -> StackState -> StackState
updateNumStack mf (StackState ns os) = (StackState (mf ns) os)

updateOpStack :: ([Op] -> [Op]) -> StackState -> StackState
updateOpStack mf (StackState ns os) = (StackState ns (mf os))

type CalcMonad = StateT StackState (Either Error)

initialState :: StackState
initialState = StackState { getNumStack = [1,2,3], getOpStack = [] }

test :: CalcMonad ()
test = do
  x <- popNum
  y <- popNum
  z <- popNum
  k <- popNum
  pushNum (z + 1)
  pushNum x
  pushNum y
  return ()

topNum :: CalcMonad Int
topNum = do
  numStack <- gets getNumStack
  case numStack of
    (n:ns) -> return n
    [] -> lift $ Left "empty number stack"

popNum :: CalcMonad Int
popNum = do
  tn <- topNum
  modify (updateNumStack tail)
  return tn

pushNum :: Int -> CalcMonad ()
pushNum n = modify (updateNumStack (n:))
