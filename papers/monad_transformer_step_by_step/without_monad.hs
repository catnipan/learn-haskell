import qualified Data.Map as Map
import ExpValueEnv

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = let ~(Just x) = Map.lookup n env in x
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e  -- env closure
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- 12+((\x -> x)(4+2))
exampleExp1 :: Exp
exampleExp1 = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- eval0 Map.empty exampleExp1
-- IntVal 18

-- (\x -> (x+10))(y)
exampleExp2 :: Exp
exampleExp2 = App (Abs "x" (Plus (Var "x") (Lit 10))) (Var "y")
-- eval0 (Map.fromList [("y", IntVal 3),("x", IntVal 20)]) exampleExp2
-- IntVal 13
-- detail below:
-- eval0 env (Abs "x" (Plus (Var "x") (Lit 10)))
-- FunVal env "x" (Plus (Var "x") (Lit 10))
-- then apply this FunVal, means evaluating the body (which is (Plus (Var "x") (Lit 10)))
-- with a env': environment add bindings "x" that maps to val2 (which is (Var "y"))
-- if any binding "x" already exists, hiding it
-- val2 now is already evaluated to (IntVal 3)