import Control.Monad

main =
    (>>=) (forM [1,2,3,4] 
            (\a ->
                ((>>) (putStrLn $ "Which color do you associate with the number " ++ show a ++ "?")
                    ((>>=) getLine 
                        (\color -> (return color))))))
        (\colors -> 
            ((>>) (putStrLn "The colors you associate with 1, 2, 3 and 4 are: ")
                (mapM putStrLn colors)))
