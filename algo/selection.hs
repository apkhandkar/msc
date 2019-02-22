-- return the smallest element in a list of Orderable
-- elements
smallest :: Ord a => [a] -> a
smallest (e:[]) = e
smallest (e:es) | e < (smallest es) = e
                | otherwise         = smallest es

-- use smallest to sort a list
