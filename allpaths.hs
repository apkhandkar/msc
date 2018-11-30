--finding paths from a node

longestpathsfrom :: Eq a => a -> [(a,a)] -> [[a]]
longestpathsfrom start graph = nextLists 
    where
      curNodes    = filter (\(f,_) -> f == start) graph
      nextStarts  = map snd curNodes
      nextLists   = if curNodes == []
                      then [[start]]
                      else map ((:) start) $ concat $ map (\nextStart -> longestpathsfrom nextStart graph) nextStarts

_subpaths :: Eq a => [[a]] -> [a] -> [[a]]
_subpaths [] [] = []
_subpaths [] (x:(y:ys)) = _subpaths ((x:[y]):[]) ys
_subpaths xs (y:ys) = _subpaths (xs++[(last xs)++[y]]) ys
_subpaths xs []     = xs

subpaths :: Eq a => [a] -> [[a]]
subpaths path = _subpaths [] path

_allpathsfrom :: Eq a => a -> [(a,a)] -> [[[a]]]
_allpathsfrom start graph = map subpaths (longestpathsfrom start graph)

_mergepaths :: Eq a => [[a]] -> [a]
_mergepaths []      = []
_mergepaths (x:xs)  = x ++ (_mergepaths xs)

_rmdupaths :: Eq a => [a] -> [a]
_rmdupaths []                   = []
_rmdupaths (x:xs) | x `elem` xs = _rmdupaths xs
                  | otherwise   = x : _rmdupaths xs

allpathsfrom :: Eq a => a -> [(a,a)] -> [[a]]
allpathsfrom start graph = _rmdupaths(_mergepaths(_allpathsfrom start graph))

--finding paths from a node
