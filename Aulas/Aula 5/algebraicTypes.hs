data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = r^2 * pi
area (Rectangle b h) = b * h

data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

addParentesis :: String -> String
addParentesis s = "(" ++ s ++ ")"

showExpr :: Expr -> String
showExpr (Lit n) = addParentesis (show n)
showExpr (Add exp1 exp2) = addParentesis (showExpr exp1 ++ "+" ++ showExpr exp2)
showExpr (Sub exp1 exp2) = addParentesis (showExpr exp1 ++ "-" ++ showExpr exp2)

data List t = Nil | Cons t (List t)

toList :: List t -> [t]
toList (Nil) = []
toList (Cons head (tail)) = head:toList tail

data Tree t = NilT |
              Node t (Tree t) (Tree t)
              deriving (Eq, Show)

height :: Tree t -> Int
height (NilT) = 0
height (Node t (tree1) (tree2)) = 1 + max (height tree1) (height tree2)

collapse :: Tree t -> [t]
collapse (NilT) = []
collapse (Node t (tree1) (tree2)) = t:(collapse tree1 ++ collapse tree2)

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f (NilT) = NilT
mapTree f (Node t (tree1) (tree2)) = Node (f t) (mapTree f tree1) (mapTree f tree2)