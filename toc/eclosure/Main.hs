module Main where

import FSMicro
import Helpers
import EClosure

main :: IO ()
main = 
    fmap lines getContents >>=
        \contents ->
            let alphabet = getAlphabet $ tokenise ';' (head contents)
                transitions = concat $ map (buildTransitionsFor alphabet 0)
                    (map (\(x,y) -> (x, map (tokenise ',') y))
                        (map (\(x,y) -> (x, tokenise ';' y))
                            (map (\x -> (head x, concat $ tail x))
                                (map (tokenise ':') (tail contents)))))
                states = inferStates transitions
                closures = zip states $ map (epsClosureT transitions) states

            in  printClosures closures >>
                return()
