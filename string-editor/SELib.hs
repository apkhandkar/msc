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
      tokenise,
      generateOutput,
      runParser ) where

import SETypes
import Data.List
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer


-- initialise (ini)

ini :: String -> StateT SEState (WriterT [Output] (Reader Char)) ()
ini i =
    state $ \_ -> ((), SEState{string=i,cursor=(length i)})

-- insert at location (ins)

insertAt :: Int -> [Char] -> [Char] -> [Char]
insertAt 1 s0 s1 = 
    s0 ++ s1
insertAt n s0 [] = 
    s0
insertAt n s0 (x:xs) =
    x:(insertAt (n-1) s0 xs)

ins :: String -> StateT SEState (WriterT [Output] (Reader Char)) ()
ins s0 = 
    state $ \SEState{string=s,cursor=c} -> ((), SEState{string=(insertAt c s0 s),cursor=(c + length s0)})

-- insert after location (ina)

insertAfter :: Int -> [Char] -> [Char] -> [Char]
insertAfter n s0 s1 = pre ++ s1 ++ post
    where
        pre = fst splits0
        post = snd splits0
        splits0 = splitAt n s0

ina :: String -> StateT SEState (WriterT [Output] (Reader Char)) ()
ina s0 =
    state $ \SEState{string=s,cursor=c} -> ((), SEState{string=(insertAfter c s s0),cursor=(c + length s0)})

-- delete (del)

deleteAt :: Int -> [Char] -> [Char]
deleteAt 1 (x:xs) =
    xs
deleteAt n [] =
    []
deleteAt n (x:xs) =
    (x:(deleteAt (n-1) xs))

del :: StateT SEState (WriterT [Output] (Reader Char)) ()
del =
    get >>= \SEState{string=s,cursor=c} ->
    if c == (length s)
        then put SEState{string=(deleteAt c s),cursor=(c-1)}
        else put SEState{string=(deleteAt c s),cursor=c}

deleteN :: Int -> StateT SEState (WriterT [Output] (Reader Char)) ()
deleteN 0 =
    return ()
deleteN n =
    del >>
    deleteN (n-1)

-- move left/right (mov l/r)

moveLeft :: Int -> StateT SEState (WriterT [Output] (Reader Char)) ()    
moveLeft n =
    get >>= \i@SEState{string=s,cursor=c} ->
    if n==0
        then put i
        else if (c-n) < 0
            then put SEState{string=s,cursor=1}
            else put SEState{string=s,cursor=(c-n)}

moveRight :: Int -> StateT SEState (WriterT [Output] (Reader Char)) ()
moveRight n =
    get >>= \i@SEState{string=s,cursor=c} ->
    if (c+n) > (length s)
        then put SEState{string=s,cursor=(length s)}
        else put SEState{string=s,cursor=(c+n)}

-- generate marker line

cursorMark :: Int -> Char -> String
cursorMark c m = (take (c-1) $ repeat ' ') ++ m:[]

-- miscellaneous functions
 
tokenise :: Char -> [Char] -> [[Char]]
tokenise _ [] = [[]]
tokenise d li = 
    map (takeWhile (/= d) . tail)
        (filter (isPrefixOf [d]) (tails (d : li)))

genOutputString :: Output -> String
genOutputString (Output (SEState{string=s,cursor=c},m)) =
    s ++ "\n" ++ (cursorMark c m)
genOutputString (Error s) =
    s

generateOutput :: [Output] -> [String]
generateOutput [] =
    []
generateOutput (o:[]) =
    (genOutputString o):[]
generateOutput (o:xs) =
    (genOutputString o):(generateOutput xs) 

runParser :: ([[String]] -> StateT SEState (WriterT [Output] (Reader Char)) ()) 
    -> [[String]] 
    -> Marker 
    -> SEState 
    -> [Output]
runParser f c r s = (\((_,_),c) -> c) $ runReader (runWriterT (runStateT (f c) s)) r
