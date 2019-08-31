module Main where
         
import FSMicro
import Helpers
import EClosure

main :: IO ()
main = 
    (>>=) (lines <$> getContents) 
        (\contents ->
            let alphabet = getAlphabet $ tokenise ';' (head contents)
                transitions = concat $ map (buildTransitionsFor alphabet 0)
-- [0]
                    (map ((\(x,y) -> (x, map (tokenise ',') y)) . (\(x,y) -> (x, tokenise ';' y)) . (\x -> (head x, concat $ tail x)) . (tokenise ':')) (tail contents))
-- [0] previously represented as:
--                    (map (\(x,y) -> (x, map (tokenise ',') y))
--                        (map (\(x,y) -> (x, tokenise ';' y))
--                            (map (\x -> (head x, concat $ tail x))
--                                (map (tokenise ':') (tail contents)))))
                states = inferStates transitions
                closures = zip states $ map (epsClosureT transitions) states

            in  printClosures closures >> 
                return ())

--  (>>=) :: Monad m => m a -> (a -> m b) -> m b, where:
--  m a = (fmap lines getContents) :: IO [String] and
--  (a -> m b) = (\contents -> ... in ... return ()) :: [String] -> IO () and thus
--  m b = main :: IO ()
