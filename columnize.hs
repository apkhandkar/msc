columnize : [[a]] -> [[a]]
columnize . ([]::xs)  = []
columnize . (x::xs)   = (map.head.(x::xs))::(columnize.(map.tail.(x::xs)))
