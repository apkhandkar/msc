f : Eq.a => [a] -> [a] -> [[a]]
f . [] . y  = []
f . (x::xs) . y | x `elem` y  = f.xs.y
                | otherwise   = (y++[x]) :: f.xs.y
