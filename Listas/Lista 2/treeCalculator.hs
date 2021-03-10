data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt n) = n
evalTree (Node op left right) = 
    case op of SUM -> evalTree left + evalTree right
               MUL -> evalTree left * evalTree right
               SUB -> evalTree left - evalTree right

main = do
    s <- getLine
    let result = evalTree (read s)
    print result