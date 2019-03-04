module EClosure
    ( epsClosureT ) where

import FSMicro

-- Given the list of transitions, generate the epsilon closure for a state
epsClosureT :: [FSMTransition] -> MState [Char] -> [MState [Char]]
epsClosureT ts start = start:nextStates
    where
        nextTos = map (\Transition{to=t} -> t) $ filter
            (\Transition{from=f,input=i} -> ((f == start) && (i == Epsilon))) ts
        nextStates = concat $ map (\nextStart -> epsClosureT ts nextStart) nextTos
