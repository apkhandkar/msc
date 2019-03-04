module Main where

import FSM
import Helpers
import EClosure

main :: IO()
main = do
    contents <- getContents
    let lContents = lines contents
    let rawInputs = tokenise ';' (head  lContents)
    let alphabet = getAlphabet rawInputs
    let tRawTrans = map (tokenise ';') (tail lContents) 
    putStrLn "Machine Alphabet: "
    printAlphabet alphabet
