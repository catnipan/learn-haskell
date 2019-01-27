{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable(foldlM)

data NFA s = NFA {
  transitionFunction :: s -> Maybe Char -> [s],
  startState :: s,
  isAccpet :: s -> Bool
}

runNFA :: forall s. NFA s -> String -> Bool
runNFA (NFA tf start isa) string = any isa (foldlM run start string >>= tfe)
  where
    tfe :: s -> [s]
    tfe st = tf st Nothing
    run :: s -> Char -> [s]
    run st c = do
      st' <- tfe st
      st'' <- tf st' (Just c)
      return st''

-- L(nfa1) is string containing a 1 in the third position from the end
nfa1 :: NFA String
nfa1 = NFA {
  transitionFunction = tf,
  startState = "q1",
  isAccpet = (=="q4")
}
  where
    tf :: String -> Maybe Char -> [String]
    tf "q1" (Just '0') = ["q1"]
    tf "q1" (Just '1') = ["q1","q2"]
    tf "q2" (Just '0') = ["q3"]
    tf "q2" (Just '1') = ["q3"]
    tf "q3" (Just '0') = ["q4"]
    tf "q3" (Just '1') = ["q4"]
    tf "q4" (Just _) = []
    tf s Nothing = [s]

-- L(nfa2) is string contains k '0's where k is a multiple of 2 or 3
nfa2 :: NFA Char
nfa2 = NFA {
  transitionFunction = tf,
  startState = 's',
  isAccpet = (\s -> s == '2' || s == '3')
}
  where
    tf :: Char -> Maybe Char -> [Char]
    tf 's' Nothing = ['2','3']
    tf '2' (Just '0') = ['a']
    tf 'a' (Just '0') = ['2']
    tf '3' (Just '0') = ['b']
    tf 'b' (Just '0') = ['c']
    tf 'c' (Just '0') = ['3']
    tf s Nothing = [s]