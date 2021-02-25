menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (max a (max b c), min a (min b c))

type Ponto = (Float,Float)
type Reta = (Ponto,Ponto)

x :: Ponto -> Float
x (p1,p2) = p1

y :: Ponto -> Float
y (p1,p2) = p2

isVertical :: Reta -> Bool
isVertical ((x1 , y1), (x2, y2)) = x1 == x2

pontoY :: Float -> Reta -> Float
pontoY x ((x1, y1), (x2, y2))
    | isVertical ((x1, y1), (x2, y2)) = 0.0
    | otherwise                       = ((y2-y1)/(x2-x1))*(x-x1)+y1