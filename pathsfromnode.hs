pathsfromnode :: Eq a => a -> [(a,a)] -> [[a]]
pathsfromnode start graph = nextLists 
    where
      curNodes    = filter (\(f,_) -> f == start) graph
      nextStarts  = map snd curNodes
      nextLists   = if curNodes == []
                      then [[start]]
                      else map ((:) start) $ concat $ map (\nextStart -> pathsfromnode nextStart graph) nextStarts
