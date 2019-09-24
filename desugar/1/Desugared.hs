import Control.Monad
import Data.Char

main = forever $
    (>>=) getLine (\l -> (putStrLn $ toUpper <$> l))
