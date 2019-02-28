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

-- the 5-tuple that is the FSM 
data FSM = FSM { 
        states      :: [MState Char], 
        alphabet    :: [ASym Char],
        transitions :: [(MState Char, ASym Char, MState Char)],
        sstate      :: MState Char,
        fstates     :: [MState Char] } 
    deriving (Show,Ord,Eq,Read)

-- get set of states of FSM
getStates :: FSM -> [MState Char]
getStates (FSM {states=s}) = s

-- get alphabet of FSM
getAlphabet :: FSM -> [ASym Char]
getAlphabet (FSM {alphabet=a}) = a

-- get transitions of FSM
getTransitions :: FSM -> [(MState Char, ASym Char, MState Char)]
getTransitions (FSM {transitions=t}) = t

_inferAlphabet :: [(MState Char, ASym Char, MState Char)] -> [ASym Char]
_inferAlphabet [] = []
_inferAlphabet ((from, input, to):[]) = input:[]
_inferAlphabet ((from, input, to):ts) = input:(_inferAlphabet ts)

_procSymList :: [ASym Char] -> [ASym Char]
_procSymList [] = []
_procSymList (a:[]) = a:[]
_procSymList (a:as) | a `elem` as  = _procSymList as
                    | otherwise    = a:(_procSymList as)

-- infer alphabet from transitions
inferAlphabet :: [(MState Char, ASym Char, MState Char)] -> [ASym Char]
inferAlphabet ts = filter (/= Epsilon) (_procSymList (_inferAlphabet ts))
