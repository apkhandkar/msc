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

