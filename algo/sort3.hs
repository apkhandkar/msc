-- cons': like (:), but adds an element to a sorted list such
-- that the sortedness of the resulting list is preserved 
cons' :: Ord a => a -> [a] -> [a]
cons' e [] = (e:[])
cons' e (x:xs) | e < x     = e:(x:xs)
               | otherwise = x:((cons') e xs)

-- sort3: sorts an unsorted list by using cons'
sort3 :: Ord a => [a] -> [a]
sort3 [] = []
sort3 (e:[]) = (e:[])
sort3 (x:xs) = ((cons') x ((sort3) xs))
