data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read, Show)

height :: Tree t -> Int
height (Nilt) = 0
height (Node t (left) (right)) = 1 + max (height left) (height right)

isBST :: Ord t => Tree t -> t -> t -> Bool
isBST Nilt _ _                    = True
isBST (Node n left right) min max = not(n < min) && not(n > max) && isBST left min n && isBST right n max

main = do
       a <- getLine
       let result = height (read a::Tree Int)
       print result