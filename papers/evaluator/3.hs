data Term = Con Int | Div Term Term deriving (Show)

answer1, error1 :: Term
answer1 = (Div (Div (Con 1972) (Con 2)) (Con 23))
error1 = (Div (Con 1) (Con 0))

-- variation two, revisited: Output
type M a = (Output, a)
type Output = String

unit :: a -> M a
unit a = ("", a)

(•) :: M a -> (a -> M b) -> M b
m • k = let (x,a) = m in
        let (y,b) = k a in
        (x ++ y, b)
out :: Output -> M ()
out x = (x,())

-- tick * λ().unit(a `quot` s)
-- to add execution traces to the monadic evaluator, take the monad as above
-- out (line(Con a) a) • λ().unit a

eval :: Term -> M Int
eval (Con a) = (out (line (Con a) a)) • (\() -> unit a)
eval term@(Div t u) = (eval t) • (\tv ->
    (eval u) • (\uv ->
        let result = tv `quot` uv
        in (out (line term result)) • (\() -> unit result)
      )
  )
-- (out (line (Div t u) (a `quot` b))) • (\() -> unit (a `quot` b))
-- eval3 :: Term -> M3 Int
-- eval3 (Con a) = (line (Con a) a, a)
-- eval3 (Div t u) =
--   let (x,a) = eval3 t in
--   let (y,b) = eval3 u in
--   (x ++ y ++ line (Div t u) (a `quot` b), (a `quot` b))

line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ "\n"