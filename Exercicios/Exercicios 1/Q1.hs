addEspOnStr :: String -> Int -> String
addEspOnStr [] _     = []
addEspOnStr [a] _    = [a]
addEspOnStr (a:as) c = a:[] ++ addEsp c ++ addEspOnStr as c

addEsp :: Int -> String
addEsp 0 = ""
addEsp n = ' ':addEsp (n - 1)