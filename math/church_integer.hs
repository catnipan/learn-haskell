type Church a = (a -> a) -> a -> a

church :: Integer -> Church Integer
church 0 = \f -> \x -> x
church n = \f -> \x -> f (church (n - 1) f x)

add :: Church Integer -> Church Integer -> Church Integer
add ci1 ci2 = \f -> \x -> (ci2 f) ((ci1 f) x)

mult :: Church Integer -> Church Integer -> Church Integer
mult ci1 ci2 = \f -> \x -> (ci1 (ci2 f)) x

succ' :: Church Integer -> Church Integer
succ' ci = \f -> \x -> f ((ci f) x)

pred' :: Church Integer -> Church Integer
pred' = undefined
-- λn.λf.λx. n (λg.λh. h (g f)) (λu. x) (λu. u)
-- pred' ci = \f -> \x -> ci ((\g -> \h -> h(g f)) (\u -> x) (\u -> u))

exp' :: Church Integer -> Church Integer -> Church Integer
exp' ci1 ci2 = \f -> \x -> (ci2 (ci1 f)) x

unchurch :: Church Integer -> Integer
unchurch cn = cn (+ 1) 0

type ChurchBool a = (a -> a -> a)

true :: ChurchBool a
true = \a -> \b -> a

false :: ChurchBool a
false = \a -> \b -> b

and' :: ChurchBool a -> ChurchBool a -> ChurchBool a
and' = undefined

or' :: ChurchBool a -> ChurchBool a -> ChurchBool a
or' = undefined

not' :: ChurchBool a -> ChurchBool a
not' = \p -> \a -> \b -> p b a

unchurchBool :: ChurchBool Bool -> Bool
unchurchBool churchbool = churchbool True False 