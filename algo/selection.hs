-- return the smallest element in a list of Orderable
-- elements
smallest :: Ord a => [a] -> a
smallest (e:[]) = e
smallest (e:es) | e < (smallest es) = e
                | otherwise         = smallest es

-- remove the first occurence of an element from a 
-- list
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove e (f:fs) | e == f    = fs
                | otherwise = (f:(remove e fs)) 

-- use smallest & remove to sort a list
selection :: Ord a => [a] -> [a]
selection [] = []
selection es = (f:(selection (remove f es)))
    where f = (smallest es) 
