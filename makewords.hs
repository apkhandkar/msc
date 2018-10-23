isWhiteSpace : [Char] -> Bool
isWhiteSpace . (x::[])  | x==' '    = True
                        | x=='\9'   = True
                        | otherwise = False
isWhiteSpace . (x::xs)  | x==' '    = True
                        | x=='\9'   = True
                        | otherwise = False
isWhiteSpace . xs                   = False
