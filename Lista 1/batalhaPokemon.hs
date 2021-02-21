getAB :: Int -> Int
getAB n = ((div n 1000) * 10) + div (mod n 1000) 100

getCD :: Int -> Int
getCD n = (div (mod n 100) 10) * 10 + mod n 10

square :: Int -> Int
square x = x*x

getBattleResult :: Int -> String
getBattleResult n = if square (getAB n + getCD n) == n 
                    then "Charmander vitorioso"
                    else "Charmander derrotado"

main = do
    input <- getLine
    let n = read input :: Int
    let result = getBattleResult n
    putStr result