doUntil . f. g . y  | g.y == False  = doUntil.f.g.(f.y)
                    | otherwise     = y
