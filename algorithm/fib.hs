data Fib = Fib { cur :: Int, nxt :: Int }

firstFib :: Fib
firstFib = (Fib 0 1)

instance Show Fib where
  show = show . get

get :: Fib -> Int
get = cur

next :: Fib -> Fib
next (Fib cur nxt) = Fib nxt (cur+nxt)

prev :: Fib -> Fib
prev (Fib cur nxt) = Fib (nxt-cur) cur

-- nth fibonacci number
fib :: Int -> Fib
fib n = (iterate next firstFib) !! n

-- get first fibonacci number not less than n
fibNLT :: Int -> Fib
fibNLT n = iter firstFib
  where iter :: Fib -> Fib
        iter fib@(Fib cur _)
          | cur >= n = fib
          | otherwise = iter (next fib)

-- get last fibonacci number not greater than n
fibNGT :: Int -> Fib
fibNGT n = iter firstFib
  where iter :: Fib -> Fib
        iter fib@(Fib cur _)
          | cur > n = prev fib
          | otherwise = iter (next fib)