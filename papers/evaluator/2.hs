data Term = Con Int | Div Term Term deriving (Show)

answer1, error1 :: Term
answer1 = (Div (Div (Con 1972) (Con 2)) (Con 23))
error1 = (Div (Con 1) (Con 0))

-- variation two, revisited: State
type M a = State -> (a, State)
type State = Int
unit :: a -> M a
unit a = \x -> (a,x) -- λx.(a,x)
(✻) :: M a -> (a -> M b) -> (M b)
m ✻ k = \x -> let (a,y) = mx in
              let (b,z) = k a y in
              (b,z)
tick :: M ()
tick = \x -> ((), x + 1)

-- as ✻ in the identity monad is function application, ✻ in the exception monad may be considered as a form of strict function application.

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) = (eval t) ✻ (\vt ->
  ((eval u) ✻ (\vu ->
    (if vu == 0
      then raise "divided by zero"
      else unit $ vt `quot` vu
    )
  )))