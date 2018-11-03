getbin' : Int -> [Int]
getbin' . 0 = [0]
getbin' . 1 = [1]
getbin' . n = (n `mod` 2) :: getbin.(n/2)

getbin . n = reverse.(getbin'.n)

btog' : [Int] -> [Int]
btog' . (x::[])       = [x]
btog' . (x::(y::ys))  = abs.(y-x) :: btog'.(y::ys)

btog . li = reverse.(btog'.(reverse.li))

