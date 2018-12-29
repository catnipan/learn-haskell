import Prelude hiding(foldl,foldr)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f start (x:xs) = foldl f (f start x) xs
foldl _ res [] = res

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f start (x:xs) = f x (foldr f start xs)
foldr _ res [] = res

-- x1:x2:x3:x4:x5:[]
-- foldl f start xs = (f (f (f (f (f start x1) x2) x3) x4) x5)
-- foldr f start xs = f x1 (f x2 (f x3 (f x4 (f x5 start))))

-- Only foldr is lazy and can be used for codata/infinite streams.
-- While foldl is tail-recursive (enhanced with strict application foldl' can avoid stack overflow).
-- This is why foldr should be used by default in Haskellin order preserve laziness across function composition. However the laziness can only be taken advantage of, if the combining function is a data constructor, which can be lazily deconstructed

-- https://gist.github.com/CMCDragonkai/9f5f75118dda10131764

-- | Monadic fold over the elements of a structure,
-- associating to the right, i.e. from right to left.
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = foldl f' return xs z0
  where f' k x z = f x z >>= k

-- | Monadic fold over the elements of a structure,
-- associating to the left, i.e. from left to right.
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr f' return xs z0
  where f' x k z = f z x >>= k