data BST t = Nilt |
               Node t (BST t) (BST t)
               deriving (Read, Show)

insert :: Ord t => BST t -> t -> BST t
insert Nilt x                = Node x Nilt Nilt
insert (Node n left right) x = if x >= n then (Node n left (insert right x)) 
                               else Node n (insert left x) right

insertList :: Ord t => BST t -> [t] -> BST t
insertList bst []       = bst
insertList (bst) (a:as) = insertList (insert bst a) as 

main = do
    a <- getLine
    b <- getLine
    let result = insertList (read a::BST Int) (read b)
    print result