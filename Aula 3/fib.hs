fib :: Int -> [Int]
fib 0 = 1:[]
fib 1 = 1:[]
fib n = (head (fib (n-1)) + head(fib (n-2))):[] ++ fib (n - 1) ++ fib(n - 2)