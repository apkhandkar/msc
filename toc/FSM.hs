module FSM 
(   ASym (Symbol, Epsilon),
    MState (State),
    FSMTransition (Transition, from, input, to),
    FSMachine (Machine, states, alphabet, transitions, sstate, fstates),
    buildFSMachine
) where

data ASym a = Symbol a | Epsilon
    deriving (Show,Ord,Eq,Read)

data MState a = State a
    deriving (Show,Ord,Eq,Read)

data FSMTransition = Transition {
        from  :: MState [Char],
        input :: ASym [Char],
        to    :: MState [Char] }
    deriving (Show,Ord,Eq,Read)

data FSMachine = Machine {
        states      :: [MState [Char]],
        alphabet    :: [ASym [Char]],
        transitions :: [FSMTransition],
        sstate      :: MState [Char],
        fstates     :: [MState [Char]] }
    deriving (Show,Ord,Eq,Read)

__inferAlphabet :: [FSMTransition] -> [ASym [Char]]
__inferAlphabet [] = []
__inferAlphabet ((Transition{input=i}):[]) = i:[]
__inferAlphabet ((Transition{input=i}):ts) = i:(__inferAlphabet ts)

_rmDup :: Eq a => [a] -> [a]
_rmDup [] = []
_rmDup (a:[]) = a:[]
_rmDup (a:as) | a `elem` as = _rmDup as
              | otherwise   = a:(_rmDup as)

_inferAlphabet :: [FSMTransition] -> [ASym [Char]]
_inferAlphabet ts = filter (/= Epsilon) (_rmDup (__inferAlphabet ts))

__inferStates :: [FSMTransition] -> [MState [Char]]
__inferStates [] = []
__inferStates ((Transition{from=f,to=t}):[]) | f == t    = f:[]
                                             | otherwise = f:t:[]
__inferStates ((Transition{from=f,to=t}):ts) | f == t    = f:(__inferStates ts)
                                             | otherwise = f:t:(__inferStates ts)

_inferStates :: [FSMTransition] -> [MState [Char]]
_inferStates ts = _rmDup (__inferStates ts)

buildFSMachine :: [FSMTransition] -> MState [Char] -> [MState [Char]] -> Maybe FSMachine
buildFSMachine ts ss fs
    | (ss `elem` inferredStates) && (all (`elem` inferredStates) fs) = Just Machine {
        states      = inferredStates,
        alphabet    = inferredAlphabet,
        transitions = ts,
        sstate      = ss,
        fstates     = fs }
    | otherwise = Nothing
    where
        inferredStates   = _inferStates ts
        inferredAlphabet = _inferAlphabet ts
