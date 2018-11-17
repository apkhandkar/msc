transfer : [Int] -> [Int]
transfer . []                         = []
transfer . (x::[])  | (0<=x)&&(x<=9)  = [x]
                    | otherwise       = x`mod`10 :: transfer.[((x-(x`mod`10))/10)]
transfer . (x::(y::ys))               = x`mod`10 :: transfer.((((x-(x`mod`10))/10)+y)::ys)


