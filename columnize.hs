columnize : [[a]] -> [[a]]
columnize . ([]::xs)  = []
columnize . xs        = (map.head.xs)::(columnize.(map.tail.xs))
