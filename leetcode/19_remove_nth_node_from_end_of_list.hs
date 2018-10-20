-- Given a linked list, remove the n-th node from the end of list and return its head.

-- Example:

-- Given linked list: 1->2->3->4->5, and n = 2.

-- After removing the second node from the end, the linked list becomes 1->2->3->5.
-- Note:

-- Given n will always be valid.

-- Follow up:

-- Could you do this in one pass?

-- removeNthFromEnd :: [a] -> Int -> [a]

type Error = String

removeNthFromEnd :: [a] -> Int -> Either Error [a]
removeNthFromEnd headNode nth = do
  fNode <- nthNode nth headNode
  return $ calc headNode fNode
  where
    calc :: [a] -> [a] -> [a]
    calc (b:bs) [] = bs
    calc (b:bs) (f:fs) = b:(calc bs fs)
    nthNode :: Int -> [a] -> Either Error [a]
    nthNode 0 xs = Right xs
    nthNode _ [] = Left $ "Barkward " ++ show nth ++ "th member not exist"
    nthNode n (x:xs) = nthNode (n-1) xs
