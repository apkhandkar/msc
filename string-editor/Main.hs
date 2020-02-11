module Main where

import SELib
import SETypes
import Text.Read
import Data.Maybe
import Control.Monad.State as S
import Control.Monad.Reader
import Control.Monad.RWS

type Marker = Char

main :: IO ()
main =
    (>>=) (lines <$> getContents)
        (\contents ->
            let 
                tokenisedContents = (tokenise ' ') <$> contents
            in 
                sequence 
--                (map putStrLn (reverse $ evalState (parser tokenisedContents []) SEState{string=" ",cursor=1,marker='^'})) >>
                (map putStrLn (snd (evalRWS (rwsParser tokenisedContents) '^' SEState'{string'=" ",cursor'=1}))) >>
                return ())

rwsParser :: [[String]] -> RWS Char [String] SEState' ()
rwsParser [] =
    return ()
rwsParser ((w:[]):ys)
    | w == "ini" =
        tell ("[Bad command: stred cannot be initialised with blank string]":[]) >>
        return ()
    | w == "out" =
        S.get >>= \SEState'{string'=s,cursor'=c} ->
        ask >>= \m ->
        tell (s:(cursorMark c m):[]) >>
        rwsParser ys
    | w == "del" =
        del' >>
        rwsParser ys 
    | otherwise =
        tell (("[Invalid instruction: " ++ w ++ "]"):[]) >>
        rwsParser ys
rwsParser ((w:x:[]):ys)
    | w == "ini" =
        ini' x >>
        rwsParser ys
    | w == "cur" =
        local (\_ -> head x) (rwsParser ys)
--  34 MINUTES:
--      local (??!! :-/) rwsParser ys
    | w == "mov" && x == "l" =
        moveLeft' 1 >> 
        rwsParser ys
    | w == "mov" && x == "r" =
        moveRight' 1 >> 
        rwsParser ys
    | w == "del" =
        let
            n = (readMaybe x :: Maybe Int)
        in
            if (n /= Nothing) then
                deleteN' (fromJust n) >> 
                rwsParser ys
            else
                tell ("[Invalid non-numeric argument to 'del', skipping this line]":[]) >>
                rwsParser ys     
    | otherwise =
        tell (("[Invalid instruction: " ++ w ++ "]"):[]) >>
        rwsParser ys
rwsParser ((w:x:xs):ys)
    | w == "ini" =
        ini' (foldl (++) x ((" "++)<$>xs)) >> 
        rwsParser ys
    | w == "mov" && x == "l" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveLeft' (fromJust n) >>  
                rwsParser ys
            else
                tell ("[Invalid non-numeric argument to 'mov l', skipping this line]":[]) >>
                rwsParser ys 
    | w == "mov" && x == "r" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveRight' (fromJust n) >> 
                rwsParser ys
            else
                tell ("[Invalid non-numeric argument to 'mov r', skipping this line]":[]) >>
                rwsParser ys 
    | w == "ins" =
        ins' (foldl (++) x ((" "++)<$>xs)) >> 
        rwsParser ys
    | w == "ina" =
        ina' (foldl (++) x ((" "++)<$>xs)) >> 
        rwsParser ys
    | otherwise =
        tell (("[Invalid instruction: " ++ w ++ "]"):[]) >>
        rwsParser ys 

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

