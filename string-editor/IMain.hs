module Main where

import SELib
import SETypes
import Text.Read
import UI.NCurses
import Data.Maybe
import System.Environment
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State as S

main :: IO ()
main = 
    head <$> getArgs >>= \f ->
    lines <$> readFile f >>= \contents ->
    return ((tokenise ' ') <$> contents) >>= \tContents ->
    return (runParser rwsParser tContents '^' SEState{string=" ",cursor=1}) >>= \output ->
    runCurses (runInteractive (zip contents output)) >> 
    return () 

runInteractive :: [(String,Output)] -> Curses (Window,Int)
runInteractive io =
    defaultWindow >>= \w ->
    (\(x,y) -> fromIntegral y) <$> screenSize >>= \cols ->
    setEcho False >>
    setCursorMode CursorInvisible >>
    newColorID ColorDefault ColorBlue 99 >>= \titleColors ->
    newColorID ColorWhite ColorBlack 98 >>= \optionsColors ->
    newColorID ColorDefault ColorRed 97 >>= \errorLabel ->
    newColorID ColorWhite ColorBlack 96 >>= \lowEmphasis ->
    updateWindow w (
        setColor titleColors >>
        moveCursor 1 4 >>
        drawString "Input from file" >>
        moveCursor 5 4 >>
        drawString "Output" >>
        moveCursor 10 4 >>
        setColor optionsColors >>
        drawString "N/n to execute next line, Q/q to quit" >>
        setColor defaultColorID) >>
    render >>
    displayOutput io w cols (errorLabel,lowEmphasis) >>
    return (w,cols)

isQuit :: Event -> Bool
isQuit ev = (ev == EventCharacter 'Q' || ev == EventCharacter 'q')

isNext :: Event -> Bool
isNext ev = (ev == EventCharacter 'N' || ev == EventCharacter 'n')



displayOutput :: [(String,Output)] -> Window -> Int -> (ColorID,ColorID) -> Curses ()
displayOutput io w cols (err,low) = loop io where
    blankLn = take cols (repeat ' ')
    loop [] = getEvent w Nothing >>= \ev ->
        case ev of
            Nothing -> loop []
            Just ev' -> if isQuit ev' then return () else loop []
    loop ((i,o):xs) = getEvent w Nothing >>= \ev ->
        case ev of
            Nothing -> loop ((i,o):xs)
            Just ev' ->
                if isQuit ev' then
                    return ()
                else if isNext ev' then
                    updateWindow w (printOutput (i,o) blankLn (err,low)) >>
                    render >>
                    loop xs 
                else
                    loop ((i,o):xs)

printOutput :: (String,Output) -> String -> (ColorID,ColorID) -> Update ()
printOutput (i,o) blankLn (err,low) =
    moveCursor 3 8 >>
    drawString blankLn >>
    moveCursor 3 8 >>
    setColor defaultColorID >>
    drawString i >>
    moveCursor 7 8 >>
    drawString blankLn >>
    moveCursor 8 8 >>
    drawString blankLn >>
    moveCursor 7 8 >>
    renderOutput o (err,low)

renderOutput :: Output -> (ColorID,ColorID) -> Update ()
renderOutput (Error str) (err,low) =
    setColor err >>
    drawString "Error" >>
    setColor defaultColorID >>
    drawString (" " ++ str)
renderOutput (NoOutput) (err,low) =
    setColor low >>
    drawString "***No Output***" >>
    setColor defaultColorID 
renderOutput out (err,low) =
    setColor defaultColorID >>
    drawString (genOutputString out)

rwsParser :: [[String]] -> StateT SEState (WriterT [Output] (Reader Char)) ()
rwsParser [] =
    return ()
rwsParser ((w:[]):ys)
    | w == "ini" =
        tell ((Error "Cannot initialise editor with blank line"):[]) >>
        return ()
    | w == "out" =
        S.get >>= \s ->
        ask >>= \m ->
        tell ((Output (s,m)):[]) >>
        rwsParser ys
    | w == "del" =
        del >>
        tell (NoOutput:[]) >>
        rwsParser ys 
    | otherwise =
        tell ((Error "Unrecognized command, skipping this line"):[]) >>
        rwsParser ys
rwsParser ((w:x:[]):ys)
    | w == "ini" =
        ini x >>
        tell (NoOutput:[]) >>
        rwsParser ys
    | w == "cur" =
        tell (NoOutput:[]) >>
        local (\_ -> head x) (rwsParser ys)
    | w == "mov" && x == "l" =
        moveLeft 1 >> 
        tell (NoOutput:[]) >>
        rwsParser ys
    | w == "mov" && x == "r" =
        moveRight 1 >> 
        tell (NoOutput:[]) >>
        rwsParser ys
    | w == "del" =
        let
            n = (readMaybe x :: Maybe Int)
        in
            if (n /= Nothing) then
                deleteN (fromJust n) >> 
                tell (NoOutput:[]) >>
                rwsParser ys
            else
                tell ((Error "Non-numeric argument to `del', skipping this line"):[]) >>
                rwsParser ys
    | otherwise =
        tell ((Error "Unrecognized command, skipping this line"):[]) >>
        rwsParser ys
rwsParser ((w:x:xs):ys)
    | w == "ini" =
        ini (foldl (++) x ((" "++)<$>xs)) >> 
        tell (NoOutput:[]) >>
        rwsParser ys
    | w == "mov" && x == "l" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveLeft (fromJust n) >>  
                tell (NoOutput:[]) >>
                rwsParser ys
            else
                tell ((Error "Non-numeric argument to `mov l', skipping this line"):[]) >>
                rwsParser ys 
    | w == "mov" && x == "r" = 
        let 
            n = (readMaybe (xs!!0) :: Maybe Int)
        in
            if (n /= Nothing) then
                moveRight (fromJust n) >> 
                tell (NoOutput:[]) >>
                rwsParser ys
            else
                tell ((Error "Non-numeric argument to `mov r', skipping this line"):[]) >>
                rwsParser ys 
    | w == "ins" =
        ins (foldl (++) x ((" "++)<$>xs)) >>
        tell (NoOutput:[]) >>
        rwsParser ys
    | w == "ina" =
        ina (foldl (++) x ((" "++)<$>xs)) >>
        tell (NoOutput:[]) >> 
        rwsParser ys
    | otherwise =
        tell ((Error "Unrecognized command, skipping this line"):[]) >>
        rwsParser ys 
