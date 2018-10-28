import Control.Monad.Identity
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import ExpValueEnv

type ErrorInfo = String
type Eval2 a = ExceptT ErrorInfo Identity a

runEval2 :: Eval2 a -> Either ErrorInfo a
runEval2 ev = runIdentity . runExceptT $ ev

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) =
  case Map.lookup n env of
    Just x -> return $ x
    Nothing -> throwError "reference error"
eval2a env (Plus e1 e2) =
  do
    IntVal i1 <- eval2a env e1
    IntVal i2 <- eval2a env e2
    return $ IntVal (i1+i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) =
  do
    val1 <- eval2a env e1
    val2 <- eval2a env e2
    case val1 of
      FunVal env' n body ->
        eval2a (Map.insert n val2 env') body

-- eval1 :: Env -> Exp -> Eval1 Value
-- eval1 env (Lit i) = return $ IntVal i
-- eval1 env (Var n) = let ~(Just x) = Map.lookup n env
--                      in return $ x
-- eval1 env (Plus e1 e2) =
--   do
--     IntVal i1 <- eval1 env e1
--     IntVal i2 <- eval1 env e2
--     return $ IntVal (i1 + i2)
-- eval1 env (Abs n e) = return $ FunVal env n e
-- eval1 env (App e1 e2) =
--   do
--     val1 <- eval1 env e1
--     val2 <- eval1 env e2
--     case val1 of
--       FunVal env' n body ->
--         eval1 (Map.insert n val2 env') body

exampleExp1 :: Exp
exampleExp1 = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- runEval1 (eval1 Map.empty exampleExp1)
-- IntVal 18

exampleExp2 :: Exp
exampleExp2 = App (Abs "x" (Plus (Var "x") (Lit 10))) (Var "y")
-- runEval1 $ eval1 (Map.fromList [("y", IntVal 3),("x", IntVal 20)]) exampleExp2
-- IntVal 13