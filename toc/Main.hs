module Main where

import FSM
import Lib
import EClosure

main :: IO()
main = do
    contents <- getContents
    let inStr = getInputsString $ lines contents
    let alphabet = getAlphabet  inStr
    putStrLn "Machine Alphabet: "
    printAlphabet alphabet
