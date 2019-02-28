data ASym a = Symbol a | Epsilon
    deriving (Show,Ord,Eq,Read)

data MState a = State a
    deriving (Show,Ord,Eq,Read)

data FSM = FSM { 
        states      :: [MState Char], 
        alphabet    :: [ASym Char],
        transitions :: [(MState Char, ASym Char, MState Char)],
        sstate      :: MState Char,
        fstates     :: [MState Char] } 
    deriving (Show,Ord,Eq,Read)
