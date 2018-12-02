data Tree a = Node a [Tree a]
            deriving (Eq,Ord,Show,Read)

pathsFromNode :: Eq a => a -> [(a,a)] -> Tree a
pathsFromNode start graph = Node start nextNodes
  where
    nextNodes = map (\(_,s) -> pathsFromNode s graph) (filter (\(f,_) -> f==start) graph)
