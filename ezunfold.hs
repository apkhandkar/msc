ezunfold : (a->a) -> a -> (a->Bool) -> [a]
ezunfold . f . s . p  | p.s == False  = (f.s) :: ezunfold.f.(f.s).p
                      | otherwise     = [] 

check.x | x > 1000 = True
        | otherwise = False