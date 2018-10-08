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

i : Eq.a => Int -> [[a]] -> [a] -> [[a]]
i . n . xs . ys | n > length.ys = []
i . 0 . xs . ys                 = []
i . 1 . xs . ys                 = xs
i . n . xs . ys                 = g.(i.(n-1).xs.ys).ys

perms : Eq.a => Int -> [a] -> [[a]]
perms . n . xs = i . n . (h.xs) . xs
