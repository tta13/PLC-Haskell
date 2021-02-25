halve :: [t] -> ([t], [t])
halve list = (map (list !!) [0,2..length list - 1], map (list !!) [1,3..length list - 1])

main = do
  x <- getLine
  print $ halve (words x)