quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x] = [x]
--quickSort (h:t)



particionaLeft :: [Int] -> Int -> [Int]
particionaLeft [] _  = []
particionaLeft (h:t) pivot
    | h <= pivot     = h:particionaLeft t pivot
    | otherwise      = particionaLeft t pivot

particionaRight :: [Int] -> Int -> [Int]
particionaRight [] _  = []
particionaRight (h:t) pivot
    | h > pivot       = h:particionaRight t pivot
    | otherwise       = particionaRight t pivot 