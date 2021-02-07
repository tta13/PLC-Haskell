double :: [Int] -> [Int]
double [] = []
double (h:tl) = 2*h:(double tl)

member :: [Int] -> Int -> Bool
member [] _ = False
member (h:tl) x
    | h == x    = True
    | otherwise = member tl x

digits :: String -> String
digits [] = ""
digits (h:tl)
    | h >= '0' && h <= '9' = h:(digits tl)
    | otherwise            = digits tl

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs (x:xs) [] = x:(sumPairs xs [])
sumPairs [] (x:xs) = x:(sumPairs [] xs)
sumPairs (x:xs) (y:ys) = (x+y):(sumPairs xs ys)

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibPairsAux :: Int -> Int -> [Int]
fibPairsAux _ 0 = []
fibPairsAux i n
    | mod (fibonacci i) 2 == 0 = (fibonacci i):(fibPairsAux (i+1) (n-1))
    | otherwise                = fibPairsAux (i+1) n

fibPairs :: Int -> [Int]
fibPairs 0 = []
fibPairs n = fibPairsAux 0 n

quickSort :: (Int -> Int -> Int) -> [Int] -> [Int]
quickSort _ []     = []
quickSort _ [x]    = [x]
quickSort function (h:tl) = quickSort (function) (particionaLeft (function) (h:tl)) ++ h:[] ++ quickSort (function) (particionaRight (function) (h:tl))

particionaLeft :: (Int -> Int -> Int) -> [Int] -> [Int]
particionaLeft function (h:tl) = [n | n <- tl, function n h <= 0]

particionaRight :: (Int -> Int -> Int) -> [Int] -> [Int]
particionaRight function (h:tl) = [n | n <- tl, function n h > 0]

getDigitsSum :: Int -> Int
getDigitsSum 0 = 0
getDigitsSum x = (mod x 10) + getDigitsSum (div x 10)

compareDigits :: Int -> Int -> Int
compareDigits x y = getDigitsSum x - getDigitsSum y

sortByDigits :: [Int] -> [Int]
sortByDigits [] = []
sortByDigits (h:tl) = quickSort (compareDigits) (h:tl)