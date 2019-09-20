module Main where
         
import FSMicro
import Helpers
import EClosure

main = 
    (>>=) (lines <$> getContents) 
        (\contents ->
            let alphabet = getAlphabet $ tokenise ';' (head contents)
                transitions = concat $ map (buildTransitionsFor alphabet 0)
                    (map ((\(x,y) -> (x, map (tokenise ',') y)) . (\(x,y) -> (x, tokenise ';' y)) . (\x -> (head x, concat $ tail x)) . (tokenise ':')) (tail contents))
                states = inferStates transitions
                closures = zip states $ map (epsClosureT transitions) states

            in  printClosures closures >> 
                return ())
