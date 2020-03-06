module Main where

import SELib
import SETypes
import Text.Read
import Data.Maybe
import System.Environment
import Control.Monad.State as S
import Control.Monad.Reader
import Control.Monad.Writer

main :: IO ()
main = 
    head <$> getArgs >>= \f ->
    lines <$> readFile f >>= \contents ->
    return ((tokenise ' ') <$> contents) >>= \tokenisedContents ->
    return (generateOutput (runParser rwsParser tokenisedContents '^' SEState{string=" ",cursor=1})) >>= \output ->
    sequence (map putStrLn output) >>
    return () 

rwsParser :: [[String]] -> StateT SEState (WriterT [Output] (Reader Char)) ()
rwsParser [] =
    return ()
rwsParser ((w:[]):ys)
    | w == "ini" =
        return ()
    | w == "out" =
        S.get >>= \s ->
        ask >>= \m ->
        tell ((Output (s,m)):[]) >>
        rwsParser ys
    | w == "del" =
        del >>
        rwsParser ys 
    | otherwise =
        tell ((Error "Unrecognized command, skipping this line"):[]) >>
        rwsParser ys
rwsParser ((w:x:[]):ys)
    | w == "ini" =
        ini x >>
        rwsParser ys
    | w == "cur" =
        local (\_ -> head x) (rwsParser ys)
    | w == "mov" && x == "l" =
        moveLeft 1 >> 
        rwsParser ys
    | w == "mov" && x == "r" =
        moveRight 1 >> 
        rwsParser ys
    | w == "del" =
        let
            n = (readMaybe x :: Maybe Int)
        in
            if (n /= Nothing) then
                deleteN (fromJust n) >> 
                rwsParser ys
            else
                rwsParser ys     
    | otherwise =
        rwsParser ys
rwsParser ((w:x:xs):ys)
    | w == "ini" =
        ini (foldl (++) x ((" "++)<$>xs)) >> 
        rwsParser ys
    | w == "mov" && x == "l" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveLeft (fromJust n) >>  
                rwsParser ys
            else
                rwsParser ys 
    | w == "mov" && x == "r" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveRight (fromJust n) >> 
                rwsParser ys
            else
                rwsParser ys 
    | w == "ins" =
        ins (foldl (++) x ((" "++)<$>xs)) >> 
        rwsParser ys
    | w == "ina" =
        ina (foldl (++) x ((" "++)<$>xs)) >> 
        rwsParser ys
    | otherwise =
        rwsParser ys 
