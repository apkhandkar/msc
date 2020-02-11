module Main where

import SELib
import SETypes
import Text.Read
import Data.Maybe
import Control.Monad.State as S
import Control.Monad.Reader

type Marker = Char

main :: IO ()
main =
    (>>=) (lines <$> getContents)
        (\contents ->
            let 
                tokenisedContents = (tokenise ' ') <$> contents
            in 
                sequence 
                (map putStrLn (reverse $ evalState (parser tokenisedContents []) SEState{string=" ",cursor=1,marker='^'})) >>
                return ())


parser :: [[String]] -> RWS Marker [String] SEState ()

parser :: [[String]] -> OutputState -> State SEState OutputState
parser [] outstate =
    return outstate
parser ((w:[]):ys) outstate
    | w == "ini" = 
        return $ "[Bad command: stred cannot be initialised with blank string]":[]
    | w == "out" = 
        S.get >>= \SEState{string=s,cursor=c,marker=m} ->
        parser ys ((cursorMark c m):s:outstate)
    | w == "del" =
        del >> (parser ys outstate)
    | otherwise =
        parser ys (("[Invalid instruction: " ++ w ++ "]"):outstate)
parser ((w:x:[]):ys) outstate
    | w == "ini" =
        ini x >> (parser ys outstate)
    | w == "cur" =
        changeCursor (head x) >> (parser ys outstate)
    | w == "mov" && x == "l" =
        moveLeft 1 >> (parser ys outstate)
    | w == "mov" && x == "r" =
        moveRight 1 >> (parser ys outstate)
    | w == "del" =
        let
            n = (readMaybe x :: Maybe Int)
        in
            if (n /= Nothing) then
                deleteN (fromJust n) >> (parser ys outstate)
            else
                parser ys ("[Invalid non-numeric argument to 'del', skipping this line]":outstate)
    | otherwise =
        parser ys (("[Invalid instruction: " ++ w ++ "]"):outstate)
parser ((w:x:xs):ys) outstate
    | w == "ini" =
        ini (foldl (++) x ((" "++)<$>xs)) >> parser ys outstate
    | w == "mov" && x == "l" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveLeft (fromJust n) >> parser ys outstate
            else
                parser ys ("[Invalid non-numeric argument to 'mov l', skipping this line]":outstate)
    | w == "mov" && x == "r" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveRight (fromJust n) >> parser ys outstate
            else
                parser ys ("[Invalid non-numeric argument to 'mov r', skipping this line]":outstate)
    | w == "ins" =
        ins (foldl (++) x ((" "++)<$>xs)) >> parser ys outstate
    | w == "ina" =
        ina (foldl (++) x ((" "++)<$>xs)) >> parser ys outstate
    | otherwise =
        parser ys (("[Invalid instruction: " ++ w ++ "]"):outstate)

