pow b 0 = 1
pow b e = b * pow b (e - 1)

imc p a = p / pow a 2

isLessThan a b c = (a < b) && (b < c)

vendas :: Int -> Int
vendas n = mod n 2

matchSales :: Int -> Int -> Int
matchSales s 0 = if vendas 0 == s then 1 else 0
matchSales s n = if vendas n == s then 1 + matchSales s (n - 1) else 0 + matchSales s (n - 1)

checkDivisors :: Int -> Int -> Bool
checkDivisors n 2 = mod n 2 == 0
checkDivisors n m = mod n m == 0 || checkDivisors n (m - 1)

isPrime :: Int -> Bool
isPrime n = not (checkDivisors n (div n 2))