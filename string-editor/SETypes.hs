module SETypes
    ( SEState (SEState, string, cursor, marker),
      SEState' (SEState', string', cursor'),
      OutputState ) where

data SEState = SEState {
    string :: [Char],
    cursor :: Int,
    marker :: Char }
    deriving (Show, Ord, Eq, Read)

data SEState' = SEState' {
    string' :: [Char],
    cursor' :: Int }
    deriving (Show, Eq)

type OutputState = [String]
