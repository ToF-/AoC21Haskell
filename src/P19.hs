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

coord :: Point -> Coord
coord p = (x p, y p, z p)

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
rotations c = map (flip rotate c) allRotations

allRotations :: [Rotation]
allRotations = [[[ 1, 0, 0] ,[ 0, 1, 0] ,[ 0, 0, 1]]
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

translation :: Coord -> Coord -> Coord
translation (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)

translate :: Coord -> Coord -> Coord
translate (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

intersection :: Scanner -> Scanner -> [(Coord,Coord)]
intersection ps qs = [(coord p, coord q) | p <- ps, q <- qs, S.size (distances p `S.intersection` distances q) >= 11]

findPosition :: Scanner -> Scanner -> Maybe (Coord,Rotation)
findPosition ps qs = case intersection ps qs of
                       [] -> Nothing
                       pairs -> findPosition' pairs
    where 
        findPosition' :: [(Coord,Coord)] -> Maybe (Coord,Rotation)
        findPosition' pairs = head <$> L.find isHomogen (map (\r -> [(translation p (rotate r q),r)| (p,q) <- pairs]) allRotations)

        isHomogen :: [(Coord,Rotation)] -> Bool
        isHomogen = (1==) . L.length . L.group . L.sort . L.map fst

acquire :: Scanner -> Scanner -> [Coord]
acquire s t = case findPosition s t of
                Nothing -> []
                Just (tr,ro) -> L.nub (L.sort ((L.map coord s) <> (map convert (map coord t))))
                    where
                        convert = (translate tr) . (rotate ro) 

acquireRange :: [Scanner] -> Int -> [Coord]
acquireRange s i = L.nub (L.sort (L.concat [acquire (s!!i) (s!!j) | j <- [0..length s - 1], j /= i]))


acquireAllRanges :: [Scanner] -> [[Coord]]
acquireAllRanges scs = [acquireRange scs i | i <- [0..length scs-1]]

acquireAll :: [Scanner] -> [[Coord]]
acquireAll scs = if isHomogen cdss then cdss else acquireAll (L.map scan cdss)
    where
        cdss = acquireAllRanges scs
        isHomogen = ((==) 1) . L.length . L.nub . L.sort . L.map length
