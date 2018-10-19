-- Given an array of integers, return indices of the two numbers such that they add up to a specific target.

-- You may assume that each input would have exactly one solution, and you may not use the same element twice.

-- Example:

-- Given nums = [2, 7, 11, 15], target = 9,

-- Because nums[0] + nums[1] = 2 + 7 = 9,
-- return [0, 1].

type Indice = Int

twoSum :: [Int] -> Int -> Maybe (Indice, Indice)
twoSum xs target = getFirstIndice indicesAnswers
  where
    idxXs = zip [0..] xs
    indicesAnswers :: [(Indice, Indice)]
    indicesAnswers = do
      (idxA, a) <- idxXs
      (idxB, b) <- drop idxA idxXs
      if a + b == target then [(idxA, idxB)] else []
    getFirstIndice (ij:_) = Just ij
    getFirstIndice [] = Nothing

-- twoSum [2, 7, 11, 15] 9
-- Just (0,1)