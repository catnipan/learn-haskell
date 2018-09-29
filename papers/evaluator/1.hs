data Term = Con Int | Div Term Term deriving (Show)

answer1, error1 :: Term
answer1 = (Div (Div (Con 1972) (Con 2)) (Con 23))
error1 = (Div (Con 1) (Con 0))

-- variation one, revisited: Exception
data M a = Raise Exception | Return a deriving (Show)
type Exception = String
unit :: a -> M a
unit a = Return a
(✻) :: M a -> (a -> M b) -> (M b)
m ✻ k = case m of
          Raise e -> raise e
          Return a -> k a
raise :: Exception -> M a
raise e = Raise e

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