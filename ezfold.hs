ezfoldl : (a->a->a) -> [a] -> a
ezfoldl . f . (x::[])       = x
ezfoldl . f . (x::(y::ys))  = ezfoldl . f . ((f.x.y)::ys)

ezfoldr : (a->a->a) -> [a] -> a
ezfoldr . f . xs = ezfoldl.f.(reverse.xs)
