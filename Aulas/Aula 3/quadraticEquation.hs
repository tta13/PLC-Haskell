delta :: (Float, Float, Float) -> Float
delta (a,b,c) = b*b - 4*a*c

quadratic :: (Float, Float, Float) -> [Float]
quadratic (a,b,c)
    | delta (a,b,c) < 0  = []
    | delta (a,b,c) == 0 = [((-1)*b + sqrt (delta(a,b,c))) / 2*a]
    | otherwise          = [((-1)*b + sqrt (delta(a,b,c))) / 2*a, ((-1)*b - (sqrt (delta(a,b,c)))) / 2*a]  
