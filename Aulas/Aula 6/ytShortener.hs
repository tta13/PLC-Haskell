import Data.List.Split

splitter = "watch?v="
shortYt = "https://youtu.be/"

shorten :: String -> String
shorten url = shortYt ++ (head (tail (splitOn splitter url)))

main :: IO ()
main =
    putStrLn "Escrevendo" >>
    writeFile "a.txt" "Hello\nworld" >>
    appendFile "a.txt" "\nof\nHaskell"
    >> putStrLn "Lendo o arquivo" >>
    readFile "a.txt" >>=
    \x -> putStrLn x