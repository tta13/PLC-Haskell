isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f x = (f x) > (f 0)

squareList :: Num a => [a] -> [a]
squareList []   = []
squareList list = map (^2) list

sumSquareList :: Num a => [a] -> a
sumSquareList list = foldr (\x acc -> x*x + acc) 0 list

keepAboveZero :: (Num a, Ord a) => [a] -> [a]
keepAboveZero list = filter (\x -> x > 0) list

newMap :: (t -> u) -> [t] -> [u]
newMap f list = foldr (\x acc -> f x : acc) [] list

newFilter :: (t -> Bool) -> [t] -> [t]
newFilter f list = foldr (\x acc -> if f x then x : acc else acc) [] list

greatestOfList x1 = foldr (\x2 acc2 -> if x2 > acc2 then x2 else acc2) (minBound :: Int) x1

greaterOfEach :: [[Int]] -> [Int]
greaterOfEach (h:tl) = foldr (\x acc -> greatestOfList x : acc) [] (h:tl)

takeWhile' :: (t -> Bool) -> [t] -> [t]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x       = x : takeWhile f xs
    | otherwise = []

dropWhile' :: (t -> Bool) -> [t] -> [t]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x       = dropWhile f xs
    | otherwise = xs 