module SETypes (
        SEState (SEState, string, cursor),
        OutputState,
        Marker,
        Output (Output, NoOutput, Error)) where

data SEState = SEState {
    string :: [Char],
    cursor :: Int }
    deriving (Show, Eq)

type OutputState = [String]
type Marker = Char

-- NoOutput isn't used in the non-interactive version of the editor

data Output = 
    Output (SEState,Marker) 
    | NoOutput
    | Error String
    deriving (Show, Eq)
