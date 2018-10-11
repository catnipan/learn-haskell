{-# LANGUAGE ExplicitForAll #-}

imager :: forall r . ((Bool -> r) -> [r])
imager = undefined

-- You are given a polymorphic function imager that, for any function from Bool to any type r, returns a list of r.

data Color = Red | Green | Blue deriving Show
data Note = C | D | E | F | G | A | B deriving Show

colorMap x = if x then Blue else Red
heatMap x = if x then 32 else 212
soundMap x = if x then C else G

main = print $ imager colorMap