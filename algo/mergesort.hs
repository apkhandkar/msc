-- app': like (++), but merges two sorted lists such that
-- the sortedness of the resulting list is mantained
app' [] [] = []
app' [] l2 = l2
app' l1 [] = l1
app' (e:l1) (f:l2) | e < f     = e : ((app') l1 (f:l2))
                   | otherwise = f : ((app') (e:l1) l2) 
