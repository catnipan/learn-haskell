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
--   (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                       (State g) = f a
--                                   in g newState

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackMainp :: State Stack Int
stackMainp = do
  push 3
  push 5
  push 99
  a <- pop
  pop

-- runState stackMainp [5,8,2,1]
-- (5,[8,2,1])