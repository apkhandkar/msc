-- A Finite State Machine (FSM) is defined as a 5-tuple:
-- (states,alphabet,transitions,sstate,fstates)
-- where:
--   states: set of states, type [MState]
--   alphabet: input alphabet, type [ASym]
--   transitions: a 3-tuple:
--     (from, input, to)
--     where:
--       from: MState <-- states
--       input: ASym <-- alphabet or Epsilon
--       to: MState <-- states
--   sstate: start state, type MState
--   fstates: set of accept states, type [MState]


-- symbols that consist of the FSM's alphabet
data ASym a  = Symbol a | Epsilon
    deriving (Show,Ord,Eq,Read)

-- states that the FSM can assume
data MState a = State a
    deriving (Show,Ord,Eq,Read)

data FSMTransition = FSMTransition {
        from  :: MState Char,
        input :: ASym Char,
        to    :: MState Char }
    deriving (Show,Ord,Eq,Read)

-- the 5-tuple that is the FSM 
data FSM = FSM { 
        states      :: [MState Char], 
        alphabet    :: [ASym Char],
        transitions :: [FSMTransition],
        sstate      :: MState Char,
        fstates     :: [MState Char] } 
    deriving (Show,Ord,Eq,Read)

_inferAlphabet :: [FSMTransition] -> [ASym Char]
_inferAlphabet [] = []
_inferAlphabet ((FSMTransition{input=i}):[]) = i:[]
_inferAlphabet ((FSMTransition{input=i}):ts) = i:(_inferAlphabet ts)

_rmDup :: Eq a => [a] -> [a]
_rmDup [] = []
_rmDup (a:[]) = a:[]
_rmDup (a:as) | a `elem` as  = _rmDup as
              | otherwise    = a:(_rmDup as)

-- infer alphabet from transitions
inferAlphabet :: [FSMTransition] -> [ASym Char]
inferAlphabet ts = filter (/= Epsilon) (_rmDup (_inferAlphabet ts))

_inferStates :: [FSMTransition] -> [MState Char]
_inferStates [] = []
_inferStates ((FSMTransition{from=f,to=t}):[]) | f == t    = f:[]
                                               | otherwise = f:t:[]
_inferStates ((FSMTransition{from=f,to=t}):ts) | f == t    = f:(_inferStates ts)
                                               | otherwise = f:t:(_inferStates ts)

-- infer states from transitions
inferStates :: [FSMTransition] -> [MState Char]
inferStates ts = _rmDup (_inferStates ts)

-- build an FSM given transitions, start state and set of final states
-- start state and final states should be from the [inferred] set of states
-- otherwise the function returns Nothing
buildFSM :: [FSMTransition] -> MState Char -> [MState Char] -> Maybe FSM
buildFSM ts ss fs 
    | (ss `elem` (inferStates ts)) && (all (`elem` (inferStates ts)) fs) = Just FSM {
        states = (inferStates ts),
        alphabet = (inferAlphabet ts),
        transitions = ts,
        sstate = ss,
        fstates = fs }
    | otherwise = Nothing

-- to debug ONLY
t1 = FSMTransition {from=State '0', input=Epsilon, to=State '1'}
t2 = FSMTransition {from=State '0', input=Epsilon, to=State '7'}
t3 = FSMTransition {from=State '1', input=Epsilon, to=State '2'}
t4 = FSMTransition {from=State '1', input=Epsilon, to=State '4'}
t5 = FSMTransition {from=State '2', input=Symbol 'a', to=State '3'}
t6 = FSMTransition {from=State '4', input=Symbol 'b', to=State '5'}
t7 = FSMTransition {from=State '3', input=Epsilon, to=State '6'}
t8 = FSMTransition {from=State '5', input=Epsilon, to=State '6'}
t9 = FSMTransition {from=State '6', input=Epsilon, to=State '1'}
t10 = FSMTransition {from=State '6', input=Epsilon, to=State '7'}
trans = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10]
-- /debug

-- this does the actual work
epsClosureT :: MState Char -> [FSMTransition] -> [MState Char]
epsClosureT start ts = start:nextLists
    where
        nextTos = map (\FSMTransition{to=t} -> t) $ filter
            (\FSMTransition{from=f,input=i} -> ((f == start) && (i == Epsilon))) ts
        nextLists = concat $ map (\nextStart -> epsClosureT nextStart ts) nextTos

-- find epsilon closure of a state from a machine
epsClosure :: MState Char -> Maybe FSM -> Maybe [MState Char]
epsClosure start Nothing = Nothing
epsClosure start (Just FSM{transitions=t}) = Just (epsClosureT start t)

