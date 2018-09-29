data Arg a = PlaceHolder | Arg a

instance (Show a) => Show (Arg a) where
  show PlaceHolder = "__"
  show (Arg a) = show a

infixl 5 <&<

(<&<) :: [Arg a] -> [Arg a] -> [Arg a]
ol <&< nl = let finalArgs = merge [] ol nl in reverse finalArgs
  where
    merge :: [Arg a] -> [Arg a] -> [Arg a] -> [Arg a]
    merge finalArgs ((Arg os):oxs) nArgs = merge ((Arg os):finalArgs) oxs nArgs
    merge finalArgs ((PlaceHolder):oxs) (PlaceHolder:nxs) = merge (PlaceHolder:finalArgs) oxs nxs
    merge finalArgs ((PlaceHolder):oxs) nArgs@((Arg nx):nxs) = merge ((Arg nx):finalArgs) oxs nxs
    merge finalArgs (os:oxs) [] = merge (os:finalArgs) oxs []
    merge finalArgs _ _ = finalArgs

argList1 = [Arg 1, PlaceHolder, Arg 3, Arg 4, PlaceHolder, PlaceHolder, Arg 5]
argList2 = [PlaceHolder, Arg 6]
argList3 = [PlaceHolder, Arg 8]
argList4 = [Arg 9, Arg 10]

-- argList1 <&< argList2
-- [1,__,3,4,6,__,5]