module Main where

import FSM
import Helpers
import EClosure

main :: IO()
main = do
    contents <- getContents

    let lContents = lines contents

    let rawInputs = tokenise ';' (head  lContents)
    let rawTransitions = map (\x -> (head $ head x, tail x)) 
                            (map (map (tokenise ',')) 
                                (map (tokenise ';') (tail lContents)))

    let alphabet = getAlphabet rawInputs
    let transitions = concat $ map (buildTransitionsFor alphabet 0) rawTransitions

    putStrLn "Machine Alphabet: "
    printAlphabet alphabet
