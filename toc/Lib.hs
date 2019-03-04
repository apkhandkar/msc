module Lib
    ( tokenise,
      getInputsString,
      getAlphabet,
      printAlphabet
    ) where

import FSM
import Data.List

tokenise :: Char -> [Char] -> [[Char]]
tokenise _ [] = [[]]
tokenise d li = 
    map (takeWhile (/= d) . tail)
        (filter (isPrefixOf [d]) (tails (d : li)))

getInputsString :: [[Char]] -> [[Char]]
getInputsString (x:xs) = tokenise ';' x

getAlphabet :: [[Char]] -> [ASym [Char]]
getAlphabet [] = []
getAlphabet (x:[]) | x == "~"  = Epsilon:[]
                   | otherwise = Symbol x:[]
getAlphabet (x:xs) | x == "~"  = Epsilon:[]
                   | otherwise = Symbol x:(getAlphabet xs)

printAlphabet :: [ASym [Char]] -> IO()
printAlphabet [] = do
    putStr "\n"
printAlphabet (x:xs) = do
    putStr $ show x ++ " "
    printAlphabet xs
