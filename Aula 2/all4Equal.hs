all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d
    | a == b && a == c && a == d = True
    | otherwise                  = False   
