-- Given an array of integers, return indices of the two numbers such that they add up to a specific target.

-- You may assume that each input would have exactly one solution, and you may not use the same element twice.

-- Example:

-- Given nums = [2, 7, 11, 15], target = 9,

-- Because nums[0] + nums[1] = 2 + 7 = 9,
-- return [0, 1].

type Indice = Int

toPair :: [a] -> [(a,a)]
toPair [] = []
toPair (x:xs) = [(x,m) | m <- xs] ++ (toPair xs)

twoSum :: [Int] -> Int -> Maybe (Indice, Indice)
twoSum xs target = getIndice . take 1 . filter isRightPair . toPair $ xis
  where xis = zip [0..] xs
        isRightPair ((_,m),(_,n)) = m + n == target
        getIndice [((i,_),(j,_))] = Just (i,j)
        getIndice [] = Nothing