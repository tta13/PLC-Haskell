tailSplit :: [Char] -> [[Char]] -> [[Char]]
tailSplit [] acc      = acc
tailSplit (h:tl) acc  = let result = takeWhile (\x -> x /= ' ') (h:tl)
                        in tailSplit (if h == ' ' then dropWhile (\y -> y == ' ') (h:tl) 
                                      else dropWhile (\y -> y /= ' ') (h:tl))
                        (if result == [] then acc
                        else acc ++ result:[])

splitWords :: String -> [String]
splitWords s = tailSplit s []
