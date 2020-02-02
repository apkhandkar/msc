module SELib
    ( ini,
      s_ini,
      ins,
      s_ins,
      ina,
      s_ina,
      insertAt,
      insertAfter,
      deleteN,
      s_deleteN,
      del,
      s_del,
      deleteAt,
      moveLeft,
      s_moveLeft,
      moveRight,
      s_moveRight,
      cursorMark,
      s_cursorMark,
      changeCursor,
      s_changeCursor,
      tokenise ) where

import SETypes
import Data.List
import Control.Monad.State

-- initialise (ini)

ini :: String -> SEState
ini str =
    (SEState{string=str,cursor=(length str),marker='^'})

s_ini :: String -> State SEState ()
s_ini i =
    state $ \_ -> ((), SEState{string=i,cursor=(length i),marker='^'})

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

s_ins :: String -> State SEState ()
s_ins s0 = 
    state $ \SEState{string=s,cursor=c,marker=m} -> ((), SEState{string=(insertAt c s0 s),cursor=(c + length s0),marker=m})

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

s_ina :: String -> State SEState ()
s_ina s0 =
    state $ \SEState{string=s,cursor=c,marker=m} -> ((), SEState{string=(insertAfter c s s0),cursor=(c + length s0),marker='^'})

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

s_del :: State SEState ()
s_del = 
    get >>= \SEState{string=s,cursor=c,marker=m} ->
    if c == (length s)
        then put SEState{string=(deleteAt c s),cursor=(c-1),marker=m}
        else put SEState{string=(deleteAt c s),cursor=c,marker=m}

deleteN :: Int -> SEState -> SEState
deleteN 0 state =
    state
deleteN n state =
    deleteN (n-1) (del state)

s_deleteN :: Int -> State SEState ()
s_deleteN 0 =
    state $ \s -> ((), s)
s_deleteN n =
    (state $ \s -> ((), snd $ runState s_del s)) >>
    s_deleteN (n-1)

-- move left/right (mov l/r)

moveLeft :: Int -> SEState -> SEState
moveLeft n state@(SEState{string=s,cursor=c,marker=m})
    | n == 0 =
        state
    | (c - n) < 0 =
        (SEState{string=s,cursor=1,marker=m})
    | otherwise =
        (SEState{string=s,cursor=(c-n),marker=m})

s_moveLeft :: Int -> State SEState ()
s_moveLeft n =
    get >>= \i@SEState{string=s,cursor=c,marker=m} ->
    if n==0
        then put i
        else if (c-n) < 0
            then put SEState{string=s,cursor=1,marker=m}
            else put SEState{string=s,cursor=(c-n),marker=m}
     

moveRight :: Int -> SEState -> SEState
moveRight n state@(SEState{string=s,cursor=c,marker=m})
    | (c + n) > (length s) = 
        (SEState{string=s,cursor=(length s),marker=m})
    | otherwise =
        (SEState{string=s,cursor=(c+n),marker=m})

s_moveRight :: Int -> State SEState ()
s_moveRight n =
    get >>= \i@SEState{string=s,cursor=c,marker=m} ->
    if (c+n) > (length s)
        then put SEState{string=s,cursor=(length s),marker=m}
        else put SEState{string=s,cursor=(c+n),marker=m}

-- change cursor character

changeCursor :: SEState -> Char -> SEState
changeCursor istate@(SEState{string=s,cursor=c,marker=m}) n =
    SEState{string=s,cursor=c,marker=n}

s_changeCursor :: Char -> State SEState ()
s_changeCursor m =
    get >>= \SEState{string=s,cursor=c,marker=_} ->
    put SEState{string=s,cursor=c,marker=m}

-- generate marker line

cursorMark :: Int -> Char -> String
cursorMark n c = (take (n-1) $ repeat ' ') ++ c:[]

s_cursorMark :: State SEState String
s_cursorMark =
    get >>= \SEState{string=_,cursor=c,marker=m} ->
    return $ (take (c-1) $ repeat ' ') ++ m:[]

-- miscellaneous functions
 
tokenise :: Char -> [Char] -> [[Char]]
tokenise _ [] = [[]]
tokenise d li = 
    map (takeWhile (/= d) . tail)
        (filter (isPrefixOf [d]) (tails (d : li)))
