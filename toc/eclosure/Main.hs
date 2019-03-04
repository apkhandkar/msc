module Main where

import FSM
import Helpers
import EClosure

main :: IO()
main = do
    contents <- getContents
    let lContents = lines contents

    let rawInputs = tokenise ';' (head  lContents)
    let rawTransitions = map (\(x,y) -> (x, map (tokenise ',') y))
                            (map (\(x,y) -> (x, tokenise ';' y))
                                (map (\x -> (head x, concat $ tail x))
                                    (map (tokenise ':') (tail lContents))))

    let alphabet = getAlphabet rawInputs
    let transitions = concat $ map (buildTransitionsFor alphabet 0) rawTransitions

    let states = inferStates transitions
    let closures = zip states (map (epsClosureT' transitions) states)

    putStrLn "Epsilon Closures: "
    printClosures closures
