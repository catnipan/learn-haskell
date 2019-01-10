{-# LANGUAGE ScopedTypeVariables #-}
import qualified Queue as Queue

data Tree a = Node a (Tree a) (Tree a) | EmptyNode deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty EmptyNode = True
isEmpty _ = False

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

emptyTree :: Tree Char
emptyTree = EmptyNode

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

data StackInfoItem a = StackInfoItem { node :: Tree a, cousin :: Tree a }

travPostI :: forall a.Tree a -> [a]
travPostI EmptyNode = []
travPostI tree = traverse $ expandNode tree []
  where
    traverse :: [StackInfoItem a] -> [a]
    traverse [] = []
    traverse ((StackInfoItem (Node a _ _) cousin):rstack) =
      a:(traverse $ (expandMaybe cousin) rstack)
      where
        expandMaybe EmptyNode = id
        expandMaybe cousin = expandNode cousin
    expandNode :: Tree a -> [StackInfoItem a] -> [StackInfoItem a]
    expandNode node stack = expand $ (StackInfoItem node EmptyNode):stack
    expand :: [StackInfoItem a] -> [StackInfoItem a]
    expand stack@((StackInfoItem (Node _ lc rc) _):_) =
      case (lc, rc) of
        (EmptyNode, EmptyNode) -> stack
        (EmptyNode, rc) -> expand $ (StackInfoItem rc EmptyNode):stack
        (lc, rc) -> expand $ (StackInfoItem lc rc):stack

-- level-order traversal (breath first search)

travLevel :: forall a.Tree a -> [a]
travLevel tree
  | isEmpty tree = []
  | otherwise = traverse (Queue.fromList [tree])
  where
    traverse :: Queue.Queue (Tree a) -> [a]
    traverse queue
      | Queue.null queue = []
      | otherwise =
          let ~((Node a lc rc), newQueue) = Queue.dequeue queue
          in a:(traverse . guardEnqueue rc . guardEnqueue lc $ newQueue)
    guardEnqueue :: Tree a -> Queue.Queue (Tree a) -> Queue.Queue (Tree a)
    guardEnqueue EmptyNode = id
    guardEnqueue node = Queue.enqueue node
