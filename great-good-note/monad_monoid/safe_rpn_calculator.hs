import Data.List
import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

foldFunc :: [Double] -> String -> Maybe [Double]
foldFunc (x:y:ys) "*" = return $ (x * y):ys
foldFunc (x:y:ys) "+" = return $ (x + y):ys
foldFunc (x:y:ys) "-" = return $ (y - x):ys
foldFunc xs numberString = liftM (:xs) (readMaybe numberString)

safeSolveRPN :: String -> Maybe Double
safeSolveRPN st = do
  [result] <- foldM foldFunc [] (words st)
  return result

-- safeSolveRPN "1 2 * 4 +"
-- Just 6.0
-- safeSolveRPN "1 2 * 4"
-- Nothing
-- 失败是因为计算到最后变为 [2,4]，do 里面的 pattern matching 失败了，调用 fail
