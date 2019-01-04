{-# LANGUAGE MultiWayIf #-}

data Tree a = Node a [Tree a]
            deriving (Eq,Ord,Show,Read)

pathsFromNode :: Eq a => a -> [(a,a)] -> Tree a
pathsFromNode start graph = Node start nextNodes
  where
    nextNodes = map (\(_,s) -> pathsFromNode s (filter (\(f,s) -> f/=start&&s/=start) graph))
                (filter (\(f,_) -> f==start) (map (\(f,s) -> if | s==start  -> (s,f) | otherwise -> (f,s)) graph))


