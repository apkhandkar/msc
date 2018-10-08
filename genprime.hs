genprime . 0 = []
genprime . 1 = [2]
genprime . 2 = [2,3]
genprime . n = genprime.(n-1) ++ [(product.(genprime.(n-1))+1)]
