data Term = Con Int | Div Term Term deriving (Show)

answer1, error1 :: Term
answer1 = (Div (Div (Con 1972) (Con 2)) (Con 23))
error1 = (Div (Con 1) (Con 0))

eval0 :: Term -> Int
eval0 (Con a) = a
eval0 (Div t u) = eval0 t `quot` eval0 u

data M1 a = Raise Exception | Return a deriving (Show)
type Exception = String

-- Variation 1: Exceptions

eval1 :: Term -> M1 Int
eval1 (Con a) = Return a
eval1 (Div t u) = case eval1 t of
                    Raise e -> Raise e
                    Return a ->
                      case eval1 u of
                        Raise e -> Raise e
                        Return b ->
                          if b == 0
                            then Raise "divide by zero"
                            else Return (a `quot` b)

-- Variation 2: State
-- count the number of divisions performed during evaluation.

type M2 a = State -> (a, State)
type State = Int

eval2 :: Term -> M2 Int
eval2 (Con a) x = (a,x)
eval2 (Div t u) x =
  let (a,y) = eval2 t x in
  let (b,z) = eval2 u y in
    (a `quot` b, z + 1)

-- 通过
-- eval2 answer1 0
-- 来计算
-- eval2 answer1 :: M2 Int
-- 类型为 State -> (Int, State)
-- 提供一个初始 State 0，
-- 计算就得到 (计算结果，最终State)

-- variation 3: Output
-- display a trace of execution

type M3 a = (Output, a)
type Output = String

eval3 :: Term -> M3 Int
eval3 (Con a) = (line (Con a) a, a)
eval3 (Div t u) =
  let (x,a) = eval3 t in
  let (y,b) = eval3 u in
  (x ++ y ++ line (Div t u) (a `quot` b), (a `quot` b))

line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ "\n"

-- putStrLn . fst . eval3 $ answer1

-- if we want to modify it to the reverse order
-- change the concat into
-- line (Div t u) (a `quot` b) ++ y ++ x

-- M stands for Monad
-- unit :: a -> M a
-- (*) :: M a -> (a -> M b) -> M b
-- A monad is a triple (M, unit, *)
-- consisting of a type constructor M and two operations: unit and (*)

-- m * λa.n
-- m and n are expressions, a is a variable
-- λa.n is a lambda expression, with the scope of bound variable a being the expression n
-- perform computation m, bind a to the resulting value, and then perform computation n
-- m :: M a
-- a :: a
-- n :: M b
-- λa.n :: a -> M b
-- like " let a = m in n "

-- eval :: Term -> M Int
-- eval (Con a) = unit a
-- eval (Div t u) = ((eval t)*(λa.((eval u) * (λb.(unit (a `quot` b))))))

-- variation zero, revisited: The basic evaluator

-- identity monad: M0' is the identity function on types
type M0' a = a
unit :: a -> M0' a
unit a = a
(*) :: M0' a -> (a -> M0' b) -> M0' b
a * k = k a