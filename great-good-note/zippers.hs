data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
  Node 'P'
    (Node 'O'
      (Node 'L'
        (Node 'N' Empty Empty)
        (Node 'T' Empty Empty)
      )
      (Node 'Y'
        (Node 'S' Empty Empty)
        (Node 'A' Empty Empty)
      )
    )
    (Node 'L'
      (Node 'W'
        (Node 'C' Empty Empty)
        (Node 'R' Empty Empty)
      )
      (Node 'A'
        (Node 'A' Empty Empty)
        (Node 'C' Empty Empty)
      )
    )

changeWToP :: Tree Char -> Tree Char
changeWToP (Node x l (Node y (Node _ m n) r)) = (Node x l (Node y (Node 'P' m n) r))

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r


elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x
-- elemAt [L,R,L] freeTree
-- 'S'

type Breadcrumbs0 = [Direction]

goLeft0 :: (Tree a, Breadcrumbs0) -> (Tree a, Breadcrumbs0)
goLeft0 (Node _ l _, bs) = (l, L:bs)

goRight0 :: (Tree a, Breadcrumbs0) -> (Tree a, Breadcrumbs0)
goRight0 (Node _ _ r, bs) = (r, R:bs)

-- goLeft0 (goRight0 (freeTree, []))
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

(-:) :: a -> (a -> b) -> b
x -: f = f x

-- (freeTree, []) -: goRight0 -: goLeft0
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')
newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

attach :: Tree a -> Zipper a -> Zipper a
attach t (_,bs) = (t,bs)

farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
newFocus3 = farLeft -: attach (Node 'Z' Empty Empty)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

-- 用在 List 结构上
type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBackward :: ListZipper a -> ListZipper a
goBackward (xs, b:bs) = (b:xs, bs)

xs = [1,2,3,4]
-- (xs,[]) -: goForward -: goForward -: goBackward
-- ([2,3,4],[1])
-- ([4],[3,2,1]) -: goBackward
-- ([3,4],[2,1])

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "goat_yelling_like_man.wmv" "baaaaa",
      File "pope_time.avi" "god bless",
      Folder "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "move eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item:rs) = break (nameIs name) items
  in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

newFocus4 = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
newFocus5 = newFocus4 -: fsUp -: fsTo "watermelon_smash.gif"

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

-- 把 pics 文件夹重命名为 cspi
newFocus6 = (myDisk,[]) -: fsTo "pics" -: fsRename "cspi" -: fsUp

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
  (Folder folderName (item:items), bs)

newFocus7 = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp