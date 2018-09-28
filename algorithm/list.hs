fillPlaceHolder :: [Maybe a] -> [Maybe a] -> [Maybe a] -> [Maybe a]
fillPlaceHolder finalArgs ((Just x):xs) (ys) =
  
-- fillPlaceHolder [Just 4, Just 5, Nothing, Just 3, Nothing, Just 4] [Just 3, Just 4]