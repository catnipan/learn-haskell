-- error :: String -> a
-- undefined :: a

-- bottom is a singular value that inhabits every type

f :: a
f = let x = x in x
-- an infinite looping term

data Foo = Foo { example1 :: Int }
-- f = Foo {}
-- will be filled with an error term by the compiler
-- f = Foo (recConError "<interactive>:xxx")