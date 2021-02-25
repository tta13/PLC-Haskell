mdc :: Int -> Int -> Int
mdc x y
    | y == 0    = x
    | otherwise = mdc y (mod x y)

primosEntreSi :: Int -> Int -> Bool
primosEntreSi n m
    | mdc n m == 1 = True
    | otherwise    = False 