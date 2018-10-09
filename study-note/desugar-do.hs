-- do notation desugaring

-- do { a <- f; m }
-- f >>= \a -> do { m }

-- do { f; m }
-- f >> do { m }

-- do { m }
-- m

-- so
-- do
--   a <- f
--   b <- g
--   c <- h
--   return (a,b,c)
-- is
-- do {
--   a <- f;
--   b <- g;
--   c <- h;
--   return (a,b,c);
-- }
-- is
-- f >>= (\a ->
--   g >>= (\b -> 
--     h >>= (\c ->
--       return (a,b,c))))

-- monad law in do notation
-- do
--   y <- return x
--   f y
-- = do f x

-- do
--   x <- m
--   return x
-- = do m

-- do
--   b <- (
--   do
--     a <-m
--     f a
--   )
--   g b
-- = do
--   a <- m
--   b <- f a
--   g b
    