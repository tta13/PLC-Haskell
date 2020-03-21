quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort [x]    = [x]
quickSort (h:tl) = quickSort (particionaLeft (h:tl)) ++ h:[] ++ quickSort (particionaRight (h:tl))



particionaLeft :: [Int] -> [Int]
particionaLeft (h:tl) = [n | n <- tl, n <= h]

particionaRight :: [Int] -> [Int]
particionaRight (h:tl) = [n | n <- tl, n > h]