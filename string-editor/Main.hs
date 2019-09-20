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
    | w == "init" =
        putStrLn "[stred initialised with blank string]" >>
        parser ys (SEState{string=" ",cursor=1})
    | w == "output" =
        output state >>
        parser ys state
    | w == "delete" =
        parser ys (deleteSE state)
    | otherwise = 
        parser ys state
parser ((w:x:[]):ys) state
    | w == "init" =
        parser ys (initSE x)
    | w == "move" && x == "l" =
        parser ys (moveLeft 1 state)
    | w == "move" && x == "r" =
        parser ys (moveRight 1 state)
    | w == "delete" =
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
    | w == "init" =
        parser ys (initSE (foldl (++) x ((" "++)<$>xs)))
    | w == "move" && x == "l" =
        let
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys (moveLeft (fromJust n) state)
            else
                putStrLn "[Invalid non-numeric argument to 'move l', skipping this line]" >>
                parser ys state
    | w == "move" && x == "r" =
        let
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                parser ys (moveRight (fromJust n) state)
            else
                putStrLn "[Invalid non-numeric argument to 'move r', skipping this line]" >>
                parser ys state
    | w == "insert" =
        parser ys (insertSE state (foldl (++) x ((" "++)<$>xs)))
    | w == "output" =
        output state >>
        parser ys state
    | otherwise =
        putStrLn ("[Invalid instruction: " ++ x ++ "]") >>
        parser ys state
