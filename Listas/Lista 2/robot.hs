data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

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

goto' :: (Int, Int) -> [Command] -> Direction
goto' orientation []     = 
       case  orientation of (0,1)  -> North
                            (0,-1) -> South
                            (1,0)  -> East
                            (-1,0) -> West
goto' orientation (x:xs) = goto' (rotate orientation x) xs

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination currentPosition cmds = goto currentPosition (0,1) cmds

faces :: Direction -> [Command] -> Direction
faces North cmd = goto' (0,1) cmd
faces South cmd = goto' (0,-1) cmd
faces East cmd  = goto' (1,0) cmd
faces West cmd  = goto' (-1,0) cmd

{- Robo 1
main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result
-}

--Robo 2
main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result