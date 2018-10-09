import System.Random
import Control.Monad.State

type Stack = [Int]

_pop :: Stack -> (Int, Stack)
_pop (x:xs) = (x, xs)

_push :: Int -> Stack -> ((), Stack)
_push a xs = ((), a:xs)

_stackMainp :: Stack -> (Int, Stack)
_stackMainp stack = let
  ((),newStack1) = _push 3 stack
  (a, newStack2) = _pop newStack1
  in _pop newStack2

-- newtype State s a = State { runState :: s -> (a,s) }

-- instance Monad (State s) where
--   return x = State $ \s -> (x,s)
--   (State h) >>= f = State $ \s ->
--     let (a, newState) = h s
--         (State g) = f a
--     in g newState

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- 这里 m 是 State s
-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- f :: a -> State s b
-- 首先我们要得到参数 a
-- 即 (a, newState) = h s
-- f a 是 (State g) :: State s b
-- g :: s -> (b, s)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

getStack' :: State Stack Stack
getStack' = state $ \s -> (s,s)

putStack' :: Stack -> State Stack ()
putStack' newState = state $ \_ -> ((), newState)

readTop :: State Stack Int
readTop = state $ \ls@(x:_) -> (x, ls)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

reverseStack4 :: State Stack ()
reverseStack4 = do
  a <- pop
  b <- pop
  c <- pop
  d <- pop
  push a
  push b
  push c
  push d
  return ()

-- runState reverseStack4 [5,8,2,1]

ifnot5push38 :: State Stack ()
ifnot5push38 = do
  a <- readTop
  if a == 5
    then return ()
    else do
      push 3
      push 8

-- runState ifnot5push38 [5,8,2,1]
-- runState ifnot5push38 [1,2,3]

ifTopIs100ThenReverse :: State Stack ()
ifTopIs100ThenReverse = do
  a <- readTop
  if a == 100
    then reverseStack4
    else return ()

-- runState ifTopIs100ThenReverse [100,3,4,5,6,4,3]
-- ((),[5,4,3,100,6,4,3])

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
    then (put [8,3,1])
    else (put [9,2,1])

stackyStack' :: State Stack ()
stackyStack' = do
  stackNow <- getStack'
  if stackNow == [1,2,3]
    then (putStack' [8,3,1])
    else (putStack' [9,2,1])


-- 使用 State Monad 处理随机性

-- random :: (Random a, RandomGen g) => g -> (a, g)
-- RandomGen g 是“状态”

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)

-- runState threeCoins (mkStdGen 33)
-- ((True,False,True),680029187 2103410263)