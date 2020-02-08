module Main where

import SELib
import SETypes
import Text.Read
import Data.Maybe
import Control.Monad.State as S

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


-- parseLine parses single line, performs IO too

parseLine :: [String] -> State SEState (IO Int)
parseLine (w:[])
    | w == "ini" =
        return $
            putStrLn "[Bad command: stred cannot be initialised with blank string]" >> 
            return 1
    | w == "out" =
        S.get >>= \SEState{string=s,cursor=c,marker=m} ->
        return $ putStrLn s >> return 0
    | w == "del" =
        del >> return (return 0)
    | otherwise =
        return $ putStrLn "[Invalid instruction]" >> return 0
parseLine (w:x:[])
    | w == "ini" =
        ini x >> return (return 0)
    | w == "cur" =
        changeCursor (head x) >> return (return 0)
    | w == "mov" && x == "l" =
        moveLeft 1 >> return (return 0)
    | w == "mov" && x == "r" =
        moveRight 1 >> return (return 0)
    | w == "del" =
        let n = readMaybe x :: Maybe Int in
        if (n /= Nothing) then
            deleteN (fromJust n) >> return (return 0)
        else
            return $
                putStrLn "[Invalid non-numeric argument to `del', skipping line]" >>
                return 0
    | otherwise =
        return $ putStrLn "[Invalid instruction]" >> return 0
parseLine (w:x:xs)
    | w == "ini" =
        ini (foldl (++) x ((" "++)<$>xs)) >> return (return 0)
    | w == "mov" =
        let n = (readMaybe (xs!!0) :: Maybe Int) in
        if (n /= Nothing) then
            if (x == "l") then
                moveLeft (fromJust n) >> return (return 0)
            else if (x == "r") then
                moveRight (fromJust n) >> return (return 0)
            else
                return $
                    putStrLn "[Invalid direction argument to mov, skipping line]" >>
                    return 0
        else
            return $
                putStrLn "[Invalid non-numeric argument to mov, skipping line]" >>
                return 0
    | w == "ins" =
        ins (foldl (++) x ((" "++)<$>xs)) >> return (return 0)
    | w == "ina" =
        ina (foldl (++) x ((" "++)<$>xs)) >> return (return 0)
    | otherwise =
        return $ putStrLn "[Invalid instruction]" >> return 0
