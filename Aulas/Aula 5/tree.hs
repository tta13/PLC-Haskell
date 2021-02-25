data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read, Show)

insert :: Ord t => Tree t -> t -> Tree t
insert Nilt x                = Node x Nilt Nilt
insert (Node n left right) x = if x >= n then (Node n left (insert right x)) 
                               else Node n (insert left x) right

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList tree []       = tree
insertList (tree) (a:as) = insertList (insert tree a) as

buildBST :: Ord t => [t] -> Tree t
buildBST []   = Nilt
buildBST list = insertList Nilt list

main = do
    a <- getLine
    b <- getLine
    let result = insertList (read a::Tree Int) (read b)
    print result