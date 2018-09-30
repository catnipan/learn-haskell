data Term = Con Int | Div Term Term deriving (Show)

answer1, error1 :: Term
answer1 = (Div (Div (Con 1972) (Con 2)) (Con 23))
error1 = (Div (Con 1) (Con 0))

-- variation two, revisited: State
type M a = State -> (a, State)
type State = Int

unit :: a -> M a
unit a = \s -> (a,s) -- λs.(a,s)
(•) :: M a -> (a -> M b) -> M b
m • k = \s -> let (a,y) = m s in
              let (b,z) = k a y in
              (b,z)
tick :: M ()
tick = \s -> ((), s+1)

-- In an impure language
-- an operation like tick would be represented by a function of type () → ().

-- add execution counts to the monadic evaluator
-- just replace unit (a `quot` b) as
-- tick * λ().unit(a `quot` s)

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = (eval t) • (\vt ->
    (eval u) • (\vu -> 
      tick • (\() -> unit (vt `quot` vu))
      )
  )

-- let (a,y) = eval t x in
-- let (b,z) = eval u y in
--   (a `quot` b, z + 1)