module Main where

import FSMicro
import Helpers
import EClosure

main :: IO()
main = do
    contents <- fmap lines getContents

    let rawInputs = tokenise ';' (head  contents)
    let rawTransitions = map (\(x,y) -> (x, map (tokenise ',') y))
                            (map (\(x,y) -> (x, tokenise ';' y))
                                (map (\x -> (head x, concat $ tail x))
                                    (map (tokenise ':') (tail contents))))

    let alphabet = getAlphabet rawInputs
    let transitions = concat $ map (buildTransitionsFor alphabet 0) rawTransitions

    let states = inferStates transitions
    let closures = zip states (map (epsClosureT transitions) states)

    printClosures closures
