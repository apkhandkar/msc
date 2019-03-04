module EClosure
    ( epsClosureT,
      epsClosureT',
      epsClosure ) where

import FSM

epsClosureT :: MState [Char] -> [FSMTransition] -> [MState [Char]]
epsClosureT start ts = start:nextStates
    where
        nextTos = map (\Transition{to=t} -> t) $ filter
            (\Transition{from=f,input=i} -> ((f == start) && (i == Epsilon))) ts
        nextStates = concat $ map (\nextStart -> epsClosureT nextStart ts) nextTos

epsClosureT' :: [FSMTransition] -> MState [Char] -> [MState [Char]]
epsClosureT' ts start = epsClosureT start ts

epsClosure :: MState [Char] -> Maybe FSMachine -> Maybe [MState [Char]]
epsClosure start Nothing = Nothing
epsClosure start (Just Machine{transitions=t}) = Just (epsClosureT start t)
