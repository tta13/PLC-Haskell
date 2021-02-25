import Data.Char

btoi :: String -> Int
btoi []  = 0
btoi bin = toInt (last bin) + 2*btoi (init bin)

toInt :: Char -> Int
toInt c = ord c - ord '0'