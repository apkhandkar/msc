module SELib
    ( initSE,
      insertSE,
      insertAt,
      deleteN,
      deleteSE,
      deleteAt,
      moveLeft,
      moveRight,
      output,
      cursorMark,
      tokenise ) where

import SETypes
import Data.List

-- init

initSE :: String -> SEState
initSE str =
    (SEState{string=str,cursor=(length str)})

-- insert

insertAt :: Int -> [Char] -> [Char] -> [Char]
insertAt 1 s0 s1 = 
    s0 ++ s1
insertAt n s0 [] = 
    s0
insertAt n s0 (x:xs) =
    x:(insertAt (n-1) s0 xs)

insertSE :: SEState -> [Char] -> SEState
insertSE state@(SEState{string=s,cursor=c}) s0 = 
    (SEState{string=(insertAt c s0 s),cursor=(c + length s0)})

-- delete

deleteAt :: Int -> [Char] -> [Char]
deleteAt 1 (x:xs) =
    xs
deleteAt n [] =
    []
deleteAt n (x:xs) =
    (x:(deleteAt (n-1) xs))

deleteSE :: SEState -> SEState
deleteSE state@(SEState{string=s,cursor=c})
    | c == (length s) =
        (SEState{string=(deleteAt c s),cursor=(c-1)})
    | otherwise =
        (SEState{string=(deleteAt c s),cursor=c})

deleteN :: Int -> SEState -> SEState
deleteN 0 state =
    state
deleteN n state =
    deleteN (n-1) (deleteSE state)

-- move left/right

moveLeft :: Int -> SEState -> SEState
moveLeft n state@(SEState{string=s,cursor=c})
    | n == 0 =
        state
    | (c - n) < 0 =
        (SEState{string=s,cursor=1})
    | otherwise =
        (SEState{string=s,cursor=(c-n)})

moveRight :: Int -> SEState -> SEState
moveRight n state@(SEState{string=s,cursor=c})
    | (c + n) > (length s) = 
        (SEState{string=s,cursor=(length s)})
    | otherwise =
        (SEState{string=s,cursor=(c+n)})

-- output

output :: SEState -> IO (SEState)
output state@(SEState{string=s,cursor=c})
    | (c > length s) = 
        putStrLn s >>
        cursorMark (length s) >>
        return state
    | otherwise = 
        putStrLn s >>
        cursorMark c >>
        return state

cursorMark :: Int -> IO ()
cursorMark 1 =
    putStrLn "^" >>
    return ()
cursorMark n    
    | n < 0 =
        cursorMark 1
    | otherwise =
        putStr " " >>
        cursorMark (n-1)

-- miscellaneous functions
 
tokenise :: Char -> [Char] -> [[Char]]
tokenise _ [] = [[]]
tokenise d li = 
    map (takeWhile (/= d) . tail)
        (filter (isPrefixOf [d]) (tails (d : li)))
