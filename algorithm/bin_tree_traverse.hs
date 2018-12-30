{-# LANGUAGE ScopedTypeVariables #-}
data Tree a = Node a (Tree a) (Tree a) | EmptyNode deriving (Show)

tree1 :: Tree Char
tree1 =
  Node 'i'
    (Node 'd'
      (Node 'c'
        (Node 'a'
          EmptyNode
          (Node 'b' EmptyNode EmptyNode)
        )
        EmptyNode
      )
      (Node 'h'
        (Node 'f'
          (Node 'e' EmptyNode EmptyNode)
          (Node 'g' EmptyNode EmptyNode)
        )
        EmptyNode
      )
    )
    (Node 'l'
      (Node 'k'
        (Node 'j' EmptyNode EmptyNode)
        EmptyNode
      )
      (Node 'n'
        (Node 'm' EmptyNode EmptyNode)
        (Node 'p'
          (Node 'o' EmptyNode EmptyNode)
          EmptyNode
        )
      )
    )
    

tree2 :: Tree Char
tree2 =
  Node 'K'
    (Node 'i'
      EmptyNode
      (Node 'h'
        (Node 'b'
          EmptyNode
          (Node 'a' EmptyNode EmptyNode)
        )
        (Node 'G'
          (Node 'e'
            (Node 'c' EmptyNode EmptyNode)
            (Node 'D' EmptyNode EmptyNode)
          )
          (Node 'F' EmptyNode EmptyNode)
        )
      )
    )
    (Node 'J' EmptyNode EmptyNode)

tree3 :: Tree Char
tree3 =
  Node 'V'
    (Node 'L' EmptyNode EmptyNode)
    (Node 'R' EmptyNode EmptyNode)

-- Pre-order traversal
-- for tree1 is "idcabhfeglkjnmpo"

travPreR :: Tree a -> [a]
travPreR EmptyNode = []
travPreR (Node a lc rc) = [a] ++ travPreR lc ++ travPreR rc

travPreI :: forall a.Tree a -> [a]
travPreI tree = traverse tree []
  where
    traverse :: Tree a -> [Tree a] -> [a]
    traverse (Node a lc rc) stack = a:(traverse lc (rc:stack))
    traverse EmptyNode (a:stack) = traverse a stack
    traverse _ [] = []

-- In-order traversal
-- for tree1 is "abcdefghijklmnop"

travInR :: Tree a -> [a]
travInR EmptyNode = []
travInR (Node a lc rc) = travInR lc ++ [a] ++ travInR rc

travInI :: forall a.Tree a -> [a]
travInI tree = traverse tree []
  where
    traverse :: Tree a -> [Tree a] -> [a]
    traverse n@(Node _ lc _) stack = traverse lc (n:stack)
    traverse EmptyNode ((Node a _ rc):stack) = a:(traverse rc stack)
    traverse _ [] = []

-- Post-order traversal
-- for tree1 is "bacegfhdjkmopnli"

travPostR :: Tree a -> [a]
travPostR EmptyNode = []
travPostR (Node a lc rc) = travPostR lc ++ travPostR rc ++ [a]

-- does the node get in the stack as a right cousin
type NeedGoToHLVFL = Bool

isEmpty :: Tree a -> Bool
isEmpty EmptyNode = True
isEmpty _ = False

travPostI :: forall a.Tree a -> [a]
travPostI tree = traverse [(tree, True)]
  where
    traverse :: [(Tree a, NeedGoToHLVFL)] -> [a]
    traverse [] = []
    traverse (((Node a _ _),False):restStack) = a:(traverse restStack)
    traverse ((topNode,True):restStack) = traverse $ gotoHLVFL ((topNode,False):restStack)

gotoHLVFL :: [(Tree a, NeedGoToHLVFL)] -> [(Tree a, NeedGoToHLVFL)]
gotoHLVFL stack@((node, _):restStack)
  | isEmpty node = restStack
  | otherwise = gotoHLVFL $ (addStackFor node) ++ stack
  where
    addStackFor :: Tree a -> [(Tree a, NeedGoToHLVFL)]
    addStackFor (Node _ EmptyNode rc) = [(rc,False)]
    addStackFor (Node _ lc EmptyNode) = [(lc,False)]
    addStackFor (Node _ lc rc) = [(lc,False),(rc,True)]