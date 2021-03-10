data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

height :: Tree t -> Int
height (Nilt) = 0
height (Node t (left) (right)) = 1 + max (height left) (height right)

main = do
       a <- getLine
       let result = height (read a::Tree Int)
       print result