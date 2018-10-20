-- Merge two sorted linked lists and return it as a new list. The new list should be made by splicing together the nodes of the first two lists.

-- Example:

-- Input: 1->2->4, 1->3->4
-- Output: 1->1->2->3->4->4

mergeTwoLists :: (Ord a) => [a] -> [a] -> [a]
mergeTwoLists la@(a:as) lb@(b:bs)
  | a <= b = a:(mergeTwoLists as lb)
  | otherwise = b:(mergeTwoLists la bs)
mergeTwoLists la [] = la
mergeTwoLists [] lb = lb