double :: [Int] -> [Int]
double [] = []
double [x] = 2*x:[]
double (x:xs) = (2*x:[]) ++ double xs 

member :: [Int] -> Int -> Bool
member [] _ = False
member (h:tl) x
    | x == h    = True
    | otherwise = member tl x

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] []     = []
sumPairs [] (x:xs) = x:[] ++ sumPairs [] xs 
sumPairs (x:xs) [] = x:[] ++ sumPairs xs []
sumPairs (x:xs) (y:ys) = (x + y):[] ++ sumPairs xs ys

digits :: String -> String
digits [] = []