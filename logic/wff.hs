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

wff31 p, q = (¬) (p ∨ q)

wff41 p = p → p
wff42 p q = (¬) ((p ∨ q) → (q ∨ p))
wff43 p q r = (q → r) → ((p ∨ q) → (p ∨ r))
wff44 p q r = (q → r) → ((p → q) → (p → r))
wff45 p q r = (p → q) → (((¬) q) → ((¬) p))
wff46 p q r = (p ∧ q) → (p ∨ q)