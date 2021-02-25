metade :: [Int] -> ([Int], [Int])
metade []     = ([], [])
metade [n]    = ([], [n])
metade (h:tl) = (take (div (length (h:tl)) 2) (h:tl), drop (div (length (h:tl)) 2) (h:tl))