module P19
    where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe

data Point = Point { x :: Integer
                   , y :: Integer
                   , z :: Integer
                   , distances :: S.Set Integer }
    deriving (Eq,Ord,Show)

type Distance = Integer
type Coord = (Integer,Integer,Integer)
type Scanner = [Point]
type Rotation = [[Integer]]

point :: Coord -> Point
point (a,b,c) = Point { x = a, y = b, z = c, distances = S.empty }

distance :: Point -> Point -> Distance
distance p1 p2 = dx2 + dy2 + dz2
    where
        dx2 = square (x p2 - x p1)
        dy2 = square (y p2 - y p1)
        dz2 = square (z p2 - z p1)
        square a = a * a

scan :: [Coord] -> [Point] 
scan cds = L.map setDistances pts
    where
        pts = map point cds
        setDistances p = p { distances = S.fromList ds }
            where
                ds = L.map (p `distance`) pts

readScanners :: [String] -> [Scanner]
readScanners = L.map (scan . (L.map toCoord)) . scannerGroups
    where 
        toCoord s = read ("("<>s<>")")
        scannerGroups = L.filter (isCoord . head) . L.groupBy (\s t -> isCoord s == isCoord t)  
        isCoord s = ',' `elem` s

rotate :: Rotation -> Coord -> Coord
rotate [[d,e,f],[g,h,i],[j,k,l]] (a,b,c) = (m,n,o)
    where
        m = a * d + b * g + c * j
        n = a * e + b * h + c * k
        o = a * f + b * i + c * l 

rotations :: Coord -> [Coord]
rotations c = map (flip rotate c) rs
    where
        rs = [[[ 1, 0, 0] ,[ 0, 1, 0] ,[ 0, 0, 1]]
             ,[[ 1, 0, 0] ,[ 0, 0,-1] ,[ 0, 1, 0]]
             ,[[ 1, 0, 0] ,[ 0,-1, 0] ,[ 0, 0,-1]]
             ,[[ 1, 0, 0] ,[ 0, 0, 1] ,[ 0,-1, 0]]
             ,[[ 0,-1, 0] ,[ 1, 0, 0] ,[ 0, 0, 1]]
             ,[[ 0, 0, 1] ,[ 1, 0, 0] ,[ 0, 1, 0]]
             ,[[ 0, 1, 0] ,[ 1, 0, 0] ,[ 0, 0,-1]]
             ,[[ 0, 0,-1] ,[ 1, 0, 0] ,[ 0,-1, 0]]
             ,[[ 1, 0, 0] ,[ 0,-1, 0] ,[ 0, 0, 1]]
             ,[[-1, 0, 0] ,[ 0, 0,-1] ,[ 0,-1, 0]]
             ,[[-1, 0, 0] ,[ 0, 1, 0] ,[ 0, 0,-1]]
             ,[[-1, 0, 0] ,[ 0, 0, 1] ,[ 0, 1, 0]]
             ,[[ 0, 1, 0] ,[-1, 0, 0] ,[ 0, 0, 1]]
             ,[[ 0, 0, 1] ,[-1, 0, 0] ,[ 0,-1, 0]]
             ,[[ 0,-1, 0] ,[-1, 0, 0] ,[ 0, 0,-1]]
             ,[[ 0, 0,-1] ,[-1, 0, 0] ,[ 0, 1, 0]]
             ,[[ 0, 0,-1] ,[ 0, 1, 0] ,[ 1, 0, 0]]
             ,[[ 0, 1, 0] ,[ 0, 0, 1] ,[ 1, 0, 0]]
             ,[[ 0, 0, 1] ,[ 0,-1, 0] ,[ 1, 0, 0]]
             ,[[ 0,-1, 0] ,[ 0, 0,-1] ,[ 1, 0, 0]]
             ,[[ 0, 0,-1] ,[ 0,-1, 0] ,[-1, 0, 0]]
             ,[[ 0,-1, 0] ,[ 0, 0, 1] ,[-1, 0, 0]]
             ,[[ 0, 0, 1] ,[ 0, 1, 0] ,[-1, 0, 0]]
             ,[[ 0, 1, 0] ,[ 0, 0,-1] ,[-1, 0, 0]]]
