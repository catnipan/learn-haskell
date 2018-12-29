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
