module SETypes
    ( SEState (SEState, string, cursor) ) where

data SEState = SEState {
    string :: [Char],
    cursor :: Int }
    deriving (Show, Ord, Eq, Read)
