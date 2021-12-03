module P02
    where


data Command = Forward Integer | Down Integer | Up Integer
    deriving (Eq, Show)

type Course = [Command]

finalPosition :: Course -> (Integer,Integer)
finalPosition = foldl executeCommand (0,0)
    where
        executeCommand (horz,depth) (Forward n) = (horz+n,depth)
        executeCommand (horz,depth) (Down n) = (horz,depth+n)
        executeCommand (horz,depth) (Up n) = (horz,depth-n)

finalPositionWithAim :: Course -> (Integer,Integer,Integer)
finalPositionWithAim = foldl executeCommand (0,0,0)
    where
        executeCommand (horz,aim,depth) (Forward n) = (horz+n,aim,depth+aim * n)
        executeCommand (horz,aim,depth) (Down n) = (horz,aim+n,depth)
        executeCommand (horz,aim,depth) (Up n) = (horz,aim-n,depth)
