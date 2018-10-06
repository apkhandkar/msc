f : [a] -> [a]
f . [a]     = [a]
f . (x::xs) = x :: x :: f.xs
