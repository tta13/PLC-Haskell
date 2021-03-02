import Data.List.Split

splitter = "watch?v="
shortYt = "https://youtu.be/"

shorten :: String -> String
shorten url = shortYt ++ (head (tail (splitOn splitter url)))

shortenList :: [String] -> [String]
shortenList []   = [""]
shortenList (u:us) = shorten u:(shortenList us)

putLn :: String -> IO ()
putLn str = putStr str >> putStr "\n"

putMultipleStr :: [String] -> IO ()
putMultipleStr []     = putStr ""
putMultipleStr (s:ss) = putLn s >> putMultipleStr ss

showShortenedUrls :: String -> IO ()
showShortenedUrls urls = putMultipleStr (shortenList (splitOn "\n" urls))

main :: IO ()
main =
    readFile "url.in" >>=
    showShortenedUrls
