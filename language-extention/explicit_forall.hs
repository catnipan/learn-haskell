{-# LANGUAGE ScopedTypeVariables #-}

import Data.List

myFunction :: Ord a => [a] -> [(a, a)]
myFunction inputList = zip sortedList nubbedList
  where sortedList = sort inputList
        nubbedList = nub inputList

-- myFunction doing OK

myFunction' :: Ord a => [a] -> [(a, a)]
myFunction' inputList = zip sortedList nubbedList
  where 
    -- sortedList :: [a]
    sortedList = sort inputList
    -- nubbedList :: [a]
    nubbedList = nub inputList

-- myFunction' will not work if we give a type to sortedList and nubbedList
-- the a's in our inner definitions are not the same as the a in our outer definition (the two inner a's aren't the same as each other, either).
-- We need a way to tell GHC that, inside the where clause, [a] does not mean forall a. [a], but instead closes over the a from the outer definition.

-- ScopedTypeVariables

myFunction'' :: forall a. Ord a => [a] -> [(a, a)]
myFunction'' inputList = zip sortedList nubbedList
  where 
    sortedList :: [a]
    sortedList = sort inputList
    nubbedList :: [a]
    nubbedList = nub inputList

-- myFunction'' [1,3,4,2,4,2,3,1]
-- [(1,1),(1,3),(2,4),(2,2)]