-- (@:): like (:), but adds an element to a sorted list such
-- that the sortedness of the resulting list is preserved 
(@:) :: Ord a => a -> [a] -> [a]
(@:) e [] = (e:[])
(@:) e (x:xs) | e < x = e:(x:xs)
               | otherwise = x:((@:) e xs)

-- sort3: sorts an unsorted list by using cons'
sort3 :: Ord a => [a] -> [a]
sort3 [] = []
sort3 (e:[]) = (e:[])
sort3 (x:xs) = (x@:((sort3) xs))
