module P19
    where

import Data.List
import Data.Maybe

data Point = Point { x :: Integer
                   , y :: Integer
                   , z :: Integer
                   , distances :: [Integer] }
    deriving (Eq,Ord,Show)

type Distance = Integer
type Coord = (Integer,Integer,Integer)
type Scanner = [Point]

point :: Coord -> Point
point (a,b,c) = Point { x = a, y = b, z = c, distances = [] }

distance :: Point -> Point -> Distance
distance p1 p2 = dx2 + dy2 + dz2
    where
        dx2 = square (x p2 - x p1)
        dy2 = square (y p2 - y p1)
        dz2 = square (z p2 - z p1)
        square a = a * a

scan :: [Coord] -> [Point] 
scan cds = map setDistances pts
    where
        pts = map point cds
        setDistances p = p { distances = tail $ sort ds }
            where
                ds = map (p `distance`) pts

readScanners :: [String] -> [Scanner]
readScanners = map (scan . (map toCoord)) . scannerGroups
    where 
        toCoord s = read ("("<>s<>")")
        scannerGroups = filter (isCoord . head) . groupBy (\s t -> isCoord s == isCoord t)  
        isCoord s = ',' `elem` s
