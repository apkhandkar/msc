exists : Eq.a => a -> [a] -> Bool
exists . a . []                   = False
exists . a . (x::xs) | a == x     = True
                     | otherwise  = exists.a.xs

toset : Eq.a => [a] -> [a]
toset . []                    = []
toset . (x::xs) | exists.x.xs = toset.xs
                | otherwise   = x :: toset.xs

isdup . (x::[]) . (y::[])                                         = True
isdup . (x::xs) . (y::ys) | exists.y.(x::xs) && exists.x.(y::ys)  = isdup.xs.ys
                          | otherwise                             = False

f . x . []                  = []
f . x . (y::ys) | isdup.x.y = True :: f.x.ys
                | otherwise = False :: f.x.ys

g . x . []                  = x :: []
g . x . (y::ys) | isdup.x.y = g.x.ys
                | otherwise = y :: g.x.ys

h . x . []                  = []
h . x . (y::ys) | x == y    = h.x.ys
                | otherwise = y :: h.x.ys


