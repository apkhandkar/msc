---
-- Stripped-down version of FSM
---

module FSMicro 
    ( ASym (Symbol, Epsilon),
      MState (State),
      FSMTransition (Transition, from, input, to),
      inferStates ) where

-- Data type for symbol(s) that make up the alphabet
data ASym a = Symbol a | Epsilon
    deriving (Show,Ord,Eq,Read)

-- Data type for state(s) that machine can assume
data MState a = State a
    deriving (Show,Ord,Eq,Read)

-- Data type for transition(s)
data FSMTransition = Transition {
        from  :: MState [Char],
        input :: ASym [Char],
        to    :: MState [Char] }
    deriving (Show,Ord,Eq,Read)

_rmDup :: Eq a => [a] -> [a]
_rmDup [] = []
_rmDup (a:[]) = a:[]
_rmDup (a:as) | a `elem` as = _rmDup as
              | otherwise   = a:(_rmDup as)

_inferStates :: [FSMTransition] -> [MState [Char]]
_inferStates [] = []
_inferStates ((Transition{from=f,to=t}):[]) | f == t    = f:[]
                                            | otherwise = f:t:[]
_inferStates ((Transition{from=f,to=t}):ts) | f == t    = f:(_inferStates ts)
                                            | otherwise = f:t:(_inferStates ts)

-- Infer machine states given the list of transitions
inferStates :: [FSMTransition] -> [MState [Char]]
inferStates ts = _rmDup (_inferStates ts)
