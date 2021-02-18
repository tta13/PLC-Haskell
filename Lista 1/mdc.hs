mdc :: Int -> Int -> Int
mdc x y
    | y == 0    = x
    | otherwise = mdc y (mod x y)

main = do
   a <- readLn
   b <- readLn
   print (mdc (a :: Int) (b :: Int))
