import System.IO

main =
    (>>=) (openFile "girlfriend.txt" ReadMode) 
        (\handle ->
            ((>>=) (hGetContents handle)
                (\contents -> 
                    ((>>) (putStr contents) (hClose handle)))))
