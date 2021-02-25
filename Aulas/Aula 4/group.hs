countElemsFromList :: Eq t => [t] -> [(t, Int)]
countElemsFromList []     = []
countElemsFromList (h:tl) = (h, sum [1 | el <- h:tl, el == h]):countElemsFromList [x | x <- tl, x /= h]

countElems :: Eq t => [[t]] -> [(t, Int)]
countElems     [] = []
countElems (h:tl) = (countElemsFromList h) ++ countElems tl

removeRedundancy :: Eq t => [(t, Int)] -> [(t, Int)]
removeRedundancy   [] = []
removeRedundancy ((x, y):tl) = (x, sum [el2 | (el1,el2) <- ((x, y):tl), el1 == x]):removeRedundancy [(el3,el4) | (el3,el4) <- tl, el3 /= x]

group :: Eq t => [[t]] -> [(t, Int)]
group l = removeRedundancy (countElems l)