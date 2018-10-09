f : Eq.a => [a] -> [a] -> [[a]]
f . y . []                    = []
f . y . (x::xs) | x `elem` y  = f.y.xs
                | otherwise   = (y++[x]) :: f.y.xs

g : Eq.a => [[a]] -> [a] -> [[a]]
g . []      . ys  = []
g . (x::xs) . ys  = f.x.ys ++ g.xs.ys

h : Eq.a => [a] -> [[a]]
h . []      = []
h . (x::xs) = [x] :: h.xs

perms : Eq.a => Int -> [a] -> [[a]]
perms . n . xs | n > length.xs  = []
perms . 0 . xs                  = []
perms . 1 . xs                  = h.xs
perms . n . xs                  = g.(perms.(n-1).xs).xs

exists : Eq.a => a -> [a] -> Bool
exists . a . []                   = False
exists . a . (x::xs) | a == x     = True
                     | otherwise  = exists.a.xs

toset : Eq.a => [a] -> [a]
toset . []                    = []
toset . (x::xs) | exists.x.xs = toset.xs
                | otherwise   = x :: toset.xs

setperms : Eq.a => Int -> [a] -> [[a]]
setperms . n . xs = perms . n . (toset.xs)
