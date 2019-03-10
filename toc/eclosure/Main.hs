module Main where

import FSMicro
import Helpers
import EClosure

main :: IO ()
main =
    fmap lines getContents >>=
        \contents ->
            let rawInputs = tokenise ';' (head contents)
                rawTransitions = map (\(x,y) -> (x, map (tokenise ',') y))
                                    (map (\(x,y) -> (x, tokenise ';' y))
                                        (map (\x -> (head x, concat $ tail x))
                                            (map (tokenise ':') (tail contents))))
                alphabet = getAlphabet rawInputs
                transitions = concat $ map (buildTransitionsFor alphabet 0) rawTransitions
                states = inferStates transitions
                closures = zip states (map (epsClosureT transitions) states)

            in  printClosures closures >>
                return ()
