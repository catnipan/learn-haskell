import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldFunc [] . words
  where foldFunc (x:y:ys) "*" = (x * y):ys
        foldFunc (x:y:ys) "+" = (x + y):ys
        foldFunc (x:y:ys) "-" = (y - x):ys
        foldFunc (x:y:ys) "/" = (y / x):ys
        foldFunc (x:y:ys) "^" = (y ** x):ys
        foldFunc (x:xs) "ln" = log x:xs
        foldFunc xs "sum" = [sum xs]
        foldFunc xs numberString = read numberString:xs