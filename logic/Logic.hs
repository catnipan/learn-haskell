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
<<<<<<< HEAD:logic/wff.hs
    in foldl accFunc True [isEqualUnderPQ p q | p <- tf, q <- tf]

wff311 p q = (¬) (p ∨ q)
wff312 p q = ((¬) p) ∧ ((¬) q)
wff313 p q = (¬)(p ∧ q)

wff321 p q = (((¬) p) ∨ q) ∧ (p ∨ ((¬) q))
wff322 p q = (p ∧ q) ∨ (((¬)p) ∧ ((¬)q))

wff33 p q = (p → q) ∧ ((¬)(p ↔ q))

wff341 p q = p → q
wff342 p q = ((¬)q) → ((¬)p)
wff343 p q = ((¬)p) → ((¬)q)
wff344 p q = q → p

wff351 p q r = p → (q → r)
wff352 p q r = (p ∧ q) → r

wff41 p = p → p
wff42 p q = (¬) ((p ∨ q) → (q ∨ p))
wff43 p q r = (q → r) → ((p ∨ q) → (p ∨ r))
wff44 p q r = (q → r) → ((p → q) → (p → r))
wff45 p q r = (p → q) → (((¬) q) → ((¬) p))
wff46 p q r = (p ∧ q) → (p ∨ q)

wffx2111 p q = (p ∧ ((¬)p)) ∨ q
wffx2112 p q = q
wffx2113 p q = p

wffx2212l p q r = (p ∨ q) ∨ r
wffx2212r p q r = p ∨ (q ∨ r)

=======
    in foldl accFunc True [isEqualUnderPQ p q | p <- tf, q <- tf]
>>>>>>> 12499ec691390270621422f8d81e175721c0b0d7:logic/Logic.hs
