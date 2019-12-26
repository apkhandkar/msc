module Main where

import SELib
import SETypes
import Text.Read
import Data.Maybe

main = do
    (>>=) (lines <$> getContents)
        (\contents ->
            let 
                tokenisedContents = (tokenise ' ') <$> contents
            in 
                parser tokenisedContents (SEState{string=" ",cursor=1}) >>
                return ())

parser :: [[[Char]]] -> SEState -> IO ()
parser [] _ = 
    return ()
parser ((w:[]):ys) state
    | w == "ini" =
    putStrLn "[Bad command: stred cannot be initialised with blank string]" >>
    return ()
--      putStrLn "[stred initialised with blank string]" >>
--      parser ys (SEState{string=" ",cursor=1})
    | w == "out" =
        output state >>
        parser ys state
    | w == "del" =
        parser ys (del state)
    | otherwise = 
        parser ys state
parser ((w:x:[]):ys) state
    | w == "ini" =
        parser ys (ini x)
    | w == "mov" && x == "l" =
        parser ys (moveLeft 1 state)
    | w == "mov" && x == "r" =
        parser ys (moveRight 1 state)
    | w == "del" =
        let 
            n = (readMaybe x :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys (deleteN (fromJust n) state)
            else
                putStrLn "[Ivalid non-numeric argument to 'delete', skipping this line]" >>
                parser ys state
    | otherwise =
        parser ys state
parser ((w:x:xs):ys) state
    | w == "ini" =
        parser ys (ini (foldl (++) x ((" "++)<$>xs)))
    | w == "mov" && x == "l" =
        let
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys (moveLeft (fromJust n) state)
            else
                putStrLn "[Invalid non-numeric argument to 'move l', skipping this line]" >>
                parser ys state
    | w == "mov" && x == "r" =
        let
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys (moveRight (fromJust n) state)
            else
                putStrLn "[Invalid non-numeric argument to 'move r', skipping this line]" >>
                parser ys state
    | w == "ins" =
        parser ys (ins state (foldl (++) x ((" "++)<$>xs)))
    | w == "ina" =
        parser ys (ina state (foldl (++) x ((" "++)<$>xs)))
    | w == "out" =
        output state >>
        parser ys state
    | otherwise =
        putStrLn ("[Invalid instruction: " ++ x ++ "]") >>
        parser ys state
