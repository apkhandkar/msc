module Helpers
    ( tokenise,
      getAlphabet,
      buildTransitionsFor,
      printClosures
    ) where

import FSMicro
import Data.List

-- Split the input line separated by delimiter(s) into list of [Char] tokens
tokenise :: Char -> [Char] -> [[Char]]
tokenise _ [] = [[]]
tokenise d li = 
    map (takeWhile (/= d) . tail)
        (filter (isPrefixOf [d]) (tails (d : li)))

-- Get the alphabet from first line of the input file
getAlphabet :: [[Char]] -> [ASym [Char]]
getAlphabet [] = []
getAlphabet (x:[]) | x == "~Eps"  = Epsilon:[]
                   | otherwise    = Symbol x:[]
getAlphabet (x:xs) | x == "~Eps"  = Epsilon:[]
                   | otherwise    = Symbol x:(getAlphabet xs)

-- Build all transitions for a state
buildTransitionsFor :: [ASym [Char]] -> Int -> ([Char],[[[Char]]]) -> [FSMTransition]
buildTransitionsFor alphabet n (from,(tos:[]))   | tos == ["-"] = 
    []
buildTransitionsFor alphabet n (from,(tos:[]))   | otherwise    =
    getTrans from tos (alphabet!!n)
buildTransitionsFor alphabet n (from,(tos:toss)) | tos == ["-"] =
    buildTransitionsFor alphabet (n+1) (from,toss) 
buildTransitionsFor alphabet n (from,(tos:toss)) | otherwise    = 
    (getTrans from tos (alphabet!!n)) ++ (buildTransitionsFor alphabet (n+1) (from,toss))

-- Convert 'raw' transition type into an FSMTransition type
getTrans :: [Char] -> [[Char]] -> ASym [Char] -> [FSMTransition]
getTrans f [] i = []
getTrans f (t:ts) i = (Transition{from=State f,input=i,to=State t}):(getTrans f ts i)

_printClosure :: (MState [Char], [MState [Char]]) -> IO()
_printClosure ((State s), (State t):[]) = do
    putStr $ t ++ " :: from " ++ s ++ "\n"
_printClosure ((State s), (State t):ts) = do
    putStr $ t ++ " "
    _printClosure ((State s), ts)

-- Print generated epsilon closures of all states
printClosures :: [(MState [Char], [MState [Char]])] -> IO()
printClosures []     = return()
printClosures (c:cs) = do
    _printClosure c
    printClosures cs
