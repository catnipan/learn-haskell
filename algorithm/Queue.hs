module Queue(
  Queue,
  fromList,
  toList,
  empty,
  null,
  enqueue,
  dequeue
) where

import Prelude hiding(null)

data Queue a = Queue {
  inStack :: [a],
  outStack :: [a]
}

instance (Show a) => Show (Queue a) where
  show queue = "Queue " ++ show (toList queue)

null :: Queue a -> Bool
null (Queue [] []) = True
null _ = False

empty :: Queue a
empty = Queue [] []

fromList :: [a] -> Queue a
fromList xs = Queue [] xs

toList :: Queue a -> [a]
toList (Queue inStack outStack) = outStack ++ reverse inStack

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue inStack outStack) = (Queue (a:inStack) outStack)

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] []) = error "dequeue an empty queue"
dequeue (Queue inStack []) = dequeue (Queue [] (reverse inStack))
dequeue (Queue inStack (x:xs)) = (x, Queue inStack xs)