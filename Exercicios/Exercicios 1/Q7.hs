isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 _     = True
isReplica [] _ _     = False
isReplica (a:as) 0 _ = False
isReplica (a:as) i c
    | a == c         = isReplica as (i - 1) c
    | otherwise      = False