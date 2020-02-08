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
      output,
      foo,
      cursorMark,
      s_cursorMark,
      changeCursor,
      tokenise ) where

import SETypes
import Data.List
import Control.Monad.State

-- initialise (ini)

ini :: String -> State SEState ()
ini i =
    state $ \_ -> ((), SEState{string=i,cursor=(length i),marker='^'})

-- insert at location (ins)

insertAt :: Int -> [Char] -> [Char] -> [Char]
insertAt 1 s0 s1 = 
    s0 ++ s1
insertAt n s0 [] = 
    s0
insertAt n s0 (x:xs) =
    x:(insertAt (n-1) s0 xs)

ins :: String -> State SEState ()
ins s0 = 
    state $ \SEState{string=s,cursor=c,marker=m} -> ((), SEState{string=(insertAt c s0 s),cursor=(c + length s0),marker=m})

-- insert after location (ina)

insertAfter :: Int -> [Char] -> [Char] -> [Char]
insertAfter n s0 s1 = pre ++ s1 ++ post
    where
        pre = fst splits0
        post = snd splits0
        splits0 = splitAt n s0

ina :: String -> State SEState ()
ina s0 =
    state $ \SEState{string=s,cursor=c,marker=m} -> ((), SEState{string=(insertAfter c s s0),cursor=(c + length s0),marker='^'})

-- delete (del)

deleteAt :: Int -> [Char] -> [Char]
deleteAt 1 (x:xs) =
    xs
deleteAt n [] =
    []
deleteAt n (x:xs) =
    (x:(deleteAt (n-1) xs))

del :: State SEState ()
del = 
    get >>= \SEState{string=s,cursor=c,marker=m} ->
    if c == (length s)
        then put SEState{string=(deleteAt c s),cursor=(c-1),marker=m}
        else put SEState{string=(deleteAt c s),cursor=c,marker=m}

deleteN :: Int -> State SEState ()
deleteN 0 =
    state $ \s -> ((), s)
deleteN n =
    (state $ \s -> ((), snd $ runState del s)) >>
    deleteN (n-1)

-- move left/right (mov l/r)

moveLeft :: Int -> State SEState ()
moveLeft n =
    get >>= \i@SEState{string=s,cursor=c,marker=m} ->
    if n==0
        then put i
        else if (c-n) < 0
            then put SEState{string=s,cursor=1,marker=m}
            else put SEState{string=s,cursor=(c-n),marker=m}
     
moveRight :: Int -> State SEState ()
moveRight n =
    get >>= \i@SEState{string=s,cursor=c,marker=m} ->
    if (c+n) > (length s)
        then put SEState{string=s,cursor=(length s),marker=m}
        else put SEState{string=s,cursor=(c+n),marker=m}

-- change cursor character

changeCursor :: Char -> State SEState ()
changeCursor m =
    get >>= \SEState{string=s,cursor=c,marker=_} ->
    put SEState{string=s,cursor=c,marker=m}

-- generate marker line

cursorMark :: Int -> Char -> String
cursorMark c m = (take (c-1) $ repeat ' ') ++ m:[]

s_cursorMark :: State SEState String
s_cursorMark =
    get >>= \SEState{string=_,cursor=c,marker=m} ->
    return $ (take (c-1) $ repeat ' ') ++ m:[]

output :: State SEState (IO ())
output =
    get >>= \SEState{string=s,cursor=c,marker=m} ->
    return $
        putStrLn s >>
        putStrLn (cursorMark c m)

foo :: State SEState (IO ())
foo = return $ putStrLn "wow"

-- miscellaneous functions
 
tokenise :: Char -> [Char] -> [[Char]]
tokenise _ [] = [[]]
tokenise d li = 
    map (takeWhile (/= d) . tail)
        (filter (isPrefixOf [d]) (tails (d : li)))
