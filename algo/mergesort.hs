-- (+++): like (++), but merges two sorted lists such that
-- the sortedness of the resulting list is mantained
(+++) :: Ord a => [a] -> [a] -> [a]
(+++) [] [] = []
(+++) [] l2 = l2
(+++) l1 [] = l1
(+++) (e:l1) (f:l2) | e < f     = e:(l1+++(f:l2))
                    | otherwise = f:((e:l1)+++l2)
