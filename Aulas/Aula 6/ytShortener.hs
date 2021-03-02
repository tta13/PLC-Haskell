import Data.List.Split

splitter = "watch?v="
shortYt = "https://youtu.be/"

shorten :: String -> String
shorten url = shortYt ++ (head (tail (splitOn splitter url)))

shortenList :: [String] -> IO()
shortenList []   = putStr ""
shortenList (u:us) = putStrLn (shorten u) >> (shortenList us)

showShortenedUrls :: String -> IO ()
showShortenedUrls urls = shortenList (splitOn "\n" urls)

main :: IO ()
main =
    readFile "url.in" >>=
    showShortenedUrls
