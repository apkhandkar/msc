module SETypes
    ( SEState (SEState, string, cursor, marker),
      OutputState ) where

data SEState = SEState {
    string :: [Char],
    cursor :: Int,
    marker :: Char }
    deriving (Show, Ord, Eq, Read)

type OutputState = [String]
