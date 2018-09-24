module Logic (
  (¬),
  (→),
  (↔),
  (∧),
  (∨),
  tf,
  testPQ,
  testPQR,
  isWffEqualPQ,
) where

(¬) :: Bool -> Bool
(¬) True = False
(¬) False = True

(→) :: Bool -> Bool -> Bool
True → False = False
_ → _ = True

(↔) :: Bool -> Bool -> Bool
True ↔ True = True
False ↔ False = True
_ ↔ _ = False

(∧) :: Bool -> Bool -> Bool
True ∧ True = True
_ ∧ _ = False

(∨) :: Bool -> Bool -> Bool
False ∨ False = False
_ ∨ _ = True

tf = [True, False]

testPQ :: (Bool -> Bool -> Bool) -> [(Bool, String, String)]
testPQ wff = [(wff p q, "P:" ++ show p, "Q:" ++ show q) | p <- tf, q <- tf]

testPQR :: (Bool -> Bool -> Bool -> Bool) -> [(Bool, String, String, String)]
testPQR wff = [(wff p q r, "P:" ++ show p, "Q:" ++ show q, "R:" ++ show r) | p <- tf, q <- tf, r <- tf]

isWffEqualPQ :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
isWffEqualPQ wff1 wff2 =
    let isEqualUnderPQ p q = (wff1 p q == wff2 p q)
        accFunc acc x = if x then acc else False
    in foldl accFunc True [isEqualUnderPQ p q | p <- tf, q <- tf]
