data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

move :: (Int, Int) -> (Int, Int) -> Command -> (Int, Int)
move (orX, orY) (posX, posY) (Forward n)  = (n*orX + posX, n*orY + posY)
move (orX, orY) (posX, posY) (Backward n) = (-n*orX + posX, -n*orY + posY)


rotate :: (Int, Int) -> Command -> (Int, Int)
rotate (orX, orY) TurnRight  = (orY, -orX)
rotate (orX, orY) TurnLeft   = (-orY, orX)
rotate orientation _         = orientation

goto :: (Int, Int) -> (Int, Int) -> [Command] -> (Int, Int)
goto pos orientation []     = pos 
goto pos orientation (x:xs) = 
       case x of TurnLeft  -> goto pos (rotate orientation x) xs
                 TurnRight -> goto pos (rotate orientation x) xs
                 _         -> goto (move orientation pos x) orientation xs

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination currentPosition cmds = goto currentPosition (0,1) cmds

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result