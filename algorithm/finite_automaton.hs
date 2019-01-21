{-# LANGUAGE ScopedTypeVariables #-}

-- a finite automaton is a 5-tuple
-- (States, Alphabet, Transition Function, start state, set of accept states)

data FiniteAutomaton s = FiniteAutomaton {
  -- states :: s,
  -- alphabet :: [Char],
  transitionFunction :: s -> Char -> s,
  startState :: s,
  isAccept :: s -> Bool
}

runFA :: forall s.(Eq s) => FiniteAutomaton s -> String -> Bool
runFA (FiniteAutomaton tf start isa) string = run start string
  where
    run :: s -> String -> Bool
    run currState [] = isa currState
    run currState (c:cs) = run (tf currState c) cs

-- construct finite automaton 1
fa1 :: FiniteAutomaton Int
fa1 = FiniteAutomaton {
  transitionFunction = tf,
  startState = 1,
  isAccept = (==2)
}
  where
    tf :: Int -> Char -> Int
    tf 1 '0' = 1
    tf 1 '1' = 2
    tf 2 '0' = 3
    tf 2 '1' = 2
    tf 3 _ = 2
    tf _ char = error $ "illegal character: " ++ [char]
-- runFA fa1 "010001"
-- True
-- L(fa1) is string contains at lease one 1 and an even number of 0s follow the last 1

-- construct finite automaton 2
fa2 :: FiniteAutomaton Int
fa2 = FiniteAutomaton {
  transitionFunction = tf,
  startState = 1,
  isAccept = (==2)
}
  where
    tf :: Int -> Char -> Int
    tf 1 '0' = 1
    tf 1 '1' = 2
    tf 2 '0' = 1
    tf 2 '1' = 2
    tf _ char = error $ "illegal character: " ++ [char]
-- L(fa2) = string ends with 1

fa4 :: FiniteAutomaton String
fa4 = FiniteAutomaton {
  transitionFunction = tf,
  startState = "q",
  isAccept = (=="q001")
}
  where
    tf :: String -> Char -> String
    tf "q" '0' = "q0"
    tf "q" '1' = "q"
    tf "q0" '0' = "q00"
    tf "q0" '1' = "q"
    tf "q00" '0' = "q00"
    tf "q00" '1' = "q001"
    tf "q001" '0' = "q001"
    tf "q001" '1' = "q001"
    tf _ char = error $ "illegal character: " ++ [char]
-- L(fa4) = all string contain 001 as a substring

fa3 :: FiniteAutomaton String
fa3 = FiniteAutomaton {
  transitionFunction = tf,
  startState = "s",
  isAccept = (`elem` ["q1","r1"])
}
  where
    tf :: String -> Char -> String
    tf "s" 'a' = "q1"
    tf "s" 'b' = "r1"
    tf "q1" 'a' = "q1"
    tf "q1" 'b' = "q2"
    tf "q2" 'b' = "q2"
    tf "q2" 'a' = "q1"
    tf "r1" 'b' = "r1"
    tf "r1" 'a' = "r2"
    tf "r2" 'a' = "r2"
    tf "r2" 'b' = "r1"
    tf _ char = error $ "illegal character: " ++ [char]
-- L(fa3) = String starts and ends with the same character

-- fa1 `union` fa2 is a new fa recognizing L(fa1) ∪ L(fa2) 
-- assuming that the two finite automaton have the same alphabet
union :: forall sa sb.(Eq sa, Eq sb) => FiniteAutomaton sa -> FiniteAutomaton sb -> FiniteAutomaton (sa, sb)
(FiniteAutomaton tfa sSa isA) `union` (FiniteAutomaton tfb sSb isB) =
  FiniteAutomaton {
    transitionFunction = tf,
    startState = (sSa, sSb),
    isAccept = (\(sa, sb) -> (isA sa || isB sb))
  }
  where
    tf :: (sa, sb) -> Char -> (sa, sb)
    tf (stateA, stateB) char = (tfa stateA char, tfb stateB char)

-- Test union operation of FA
-- fa12 :: FiniteAutomaton (Int, Int)
-- fa12 = fa1 `union` fa2
-- runFA (fa1 `union` fa4) "01010110"
-- False