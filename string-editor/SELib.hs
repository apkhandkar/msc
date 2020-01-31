module SELib
    ( ini,
      ins,
      ina,
      insertAt,
      insertAfter,
      deleteN,
      del,
      deleteAt,
      moveLeft,
      moveRight,
      cursorMark,
      changeCursor,
      tokenise ) where

import SETypes
import Data.List

-- initialise (ini)

ini :: String -> SEState
ini str =
    (SEState{string=str,cursor=(length str),marker='^'})

-- insert at location (ins)

insertAt :: Int -> [Char] -> [Char] -> [Char]
insertAt 1 s0 s1 = 
    s0 ++ s1
insertAt n s0 [] = 
    s0
insertAt n s0 (x:xs) =
    x:(insertAt (n-1) s0 xs)

ins :: SEState -> [Char] -> SEState
ins state@(SEState{string=s,cursor=c,marker=m}) s0 = 
    (SEState{string=(insertAt c s0 s),cursor=(c + length s0),marker=m})

-- insert after location (ina)

insertAfter :: Int -> [Char] -> [Char] -> [Char]
insertAfter n s0 s1 = pre ++ s1 ++ post
    where
        pre = fst splits0
        post = snd splits0
        splits0 = splitAt n s0

ina :: SEState -> [Char] -> SEState
ina state@(SEState{string=s,cursor=c}) s0 = 
    (SEState{string=(insertAfter c s s0),cursor=(c + length s0),marker='^'})

-- delete (del)

deleteAt :: Int -> [Char] -> [Char]
deleteAt 1 (x:xs) =
    xs
deleteAt n [] =
    []
deleteAt n (x:xs) =
    (x:(deleteAt (n-1) xs))

del :: SEState -> SEState
del state@(SEState{string=s,cursor=c,marker=m})
    | c == (length s) =
        (SEState{string=(deleteAt c s),cursor=(c-1),marker=m})
    | otherwise =
        (SEState{string=(deleteAt c s),cursor=c,marker=m})

deleteN :: Int -> SEState -> SEState
deleteN 0 state =
    state
deleteN n state =
    deleteN (n-1) (del state)

-- move left/right (mov l/r)

moveLeft :: Int -> SEState -> SEState
moveLeft n state@(SEState{string=s,cursor=c,marker=m})
    | n == 0 =
        state
    | (c - n) < 0 =
        (SEState{string=s,cursor=1,marker=m})
    | otherwise =
        (SEState{string=s,cursor=(c-n),marker=m})

moveRight :: Int -> SEState -> SEState
moveRight n state@(SEState{string=s,cursor=c,marker=m})
    | (c + n) > (length s) = 
        (SEState{string=s,cursor=(length s),marker=m})
    | otherwise =
        (SEState{string=s,cursor=(c+n),marker=m})

-- change cursor character

changeCursor :: SEState -> Char -> SEState
changeCursor istate@(SEState{string=s,cursor=c,marker=m}) n =
    SEState{string=s,cursor=c,marker=n}

-- generate marker line

cursorMark :: Int -> Char -> String
cursorMark n c = (take (n-1) $ repeat ' ') ++ c:[]

-- miscellaneous functions
 
tokenise :: Char -> [Char] -> [[Char]]
tokenise _ [] = [[]]
tokenise d li = 
    map (takeWhile (/= d) . tail)
        (filter (isPrefixOf [d]) (tails (d : li)))
