import Logic

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

testFiveWFF :: (Bool, Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool, Bool)
testFiveWFF (p1, p2, p3, p4, p5) = (np1, np2, np3, np4, np5)
  where np1 = p2 ∧ p3 ∧ p4 ∧ p5
        np2 = ((¬) p3) ∧ ((¬) p4) ∧ ((¬) p5)
        np3 = p1 ∧ p2
        np4 = p1 ∨ p2 ∨ p3
        np5 = ((¬) p1) ∧ ((¬) p2) ∧ ((¬) p3) ∧ ((¬) p4)