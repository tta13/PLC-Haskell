data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

move :: (Int, Int) -> (Int, Int) -> Command -> (Int, Int)
move (orX, orY) (posX, posY) (Forward n)  = (n*orX + posX, n*orY + posY)
move (orX, orY) (posX, posY) (Backward n) = (-n*orX + posX, -n*orY + posY)
move orientation position _ = position

rotate :: (Int, Int) -> Command -> (Int, Int)
rotate (orX, orY) TurnRight  = (orY, -orX)
rotate (orX, orY) TurnLeft   = (-orY, orX)
rotate orientation _         = orientation

goto :: (Int, Int) -> (Int, Int) -> [Command] -> (Int, Int)
goto pos orientation []     = pos 
goto pos orientation (x:xs) = let newOri = rotate orientation x
                              in goto (move newOri pos x) newOri xs 

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination currentPosition cmds = goto currentPosition (0,1) cmds

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result