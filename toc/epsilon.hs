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
data ASym a = Symbol a | Epsilon
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

