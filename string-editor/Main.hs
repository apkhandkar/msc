module Main where

import SELib
import SETypes
import Text.Read
import Data.Maybe

main :: IO ()
main =
    (>>=) (lines <$> getContents)
        (\contents ->
            let 
                tokenisedContents = (tokenise ' ') <$> contents
            in 
                sequence (map putStrLn (reverse $ parser tokenisedContents [] SEState{string=" ",cursor=1,marker='^'})) >>
                return ())

parser :: [[String]] -> OutputState -> SEState -> OutputState
parser [] outstate _ = 
    outstate
parser ((w:[]):ys) outstate state@(SEState{string=s,cursor=c,marker=m})
    | w == "ini" =
        ("[Bad command: stred cannot be initialised with blank string]":[])
    | w == "out" =
        parser ys ((cursorMark c m):s:outstate) state
    | w == "del" =
        parser ys outstate (del state)
    | otherwise = 
        parser ys (("[Invalid instruction: " ++ w ++ "]"):outstate) state
parser ((w:x:[]):ys) outstate state
    | w == "ini" =
        parser ys outstate (ini x)
    | w == "cur" =
        parser ys outstate (changeCursor state (head x))
    | w == "mov" && x == "l" =
        parser ys outstate (moveLeft 1 state)
    | w == "mov" && x == "r" =
        parser ys outstate (moveRight 1 state)
    | w == "del" =
        let 
            n = (readMaybe x :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys outstate (deleteN (fromJust n) state)
            else
                parser ys ("[Invalid non-numeric argument to 'delete', skipping this line]":outstate) state
    | otherwise =
        parser ys (("[Invalid instruction: " ++ w ++ "]"):outstate) state
parser ((w:x:xs):ys) outstate state@(SEState{string=s,cursor=c,marker=m})
    | w == "ini" =
        parser ys outstate (ini (foldl (++) x ((" "++)<$>xs)))
    | w == "mov" && x == "l" =
        let
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys outstate (moveLeft (fromJust n) state)
            else
                parser ys ("[Invalid non-numeric argument to 'move l', skipping this line]":outstate) state
    | w == "mov" && x == "r" =
        let
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys outstate (moveRight (fromJust n) state)
            else
                parser ys ("[Invalid non-numeric argument to 'move r', skipping this line]":outstate) state 
    | w == "ins" =
        parser ys outstate (ins state (foldl (++) x ((" "++)<$>xs)))
    | w == "ina" =
        parser ys outstate (ina state (foldl (++) x ((" "++)<$>xs)))
    | w == "out" =
        parser ys ((cursorMark c m):s:outstate) state
    | otherwise =
        parser ys (("[Invalid instruction: " ++ w ++ "]"):outstate) state
