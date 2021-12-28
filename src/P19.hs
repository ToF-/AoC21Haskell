module P19
    where

import Data.List 
import Data.Maybe
import Data.Function
import Data.Ord

data Point = Point { x :: Integer
                   , y :: Integer
                   , z :: Integer
                   , is000 :: Bool
                   , distances :: [Integer] }
    deriving (Eq,Ord,Show)

type Distance = Integer
type Coord = (Integer,Integer,Integer)
type Scanner = [Point]
type Rotation = [[Integer]]

point :: Coord -> Point
point (0,0,0) = Point { x = 0, y = 0, z = 0, is000 = True, distances = [] } 
point (a,b,c) = Point { x = a, y = b, z = c, is000 = False, distances = [] }

coord :: Point -> Coord
coord p = (x p, y p, z p)

distance :: Point -> Point -> Distance
distance p1 p2 = dx2 + dy2 + dz2
    where
        dx2 = square (x p2 - x p1)
        dy2 = square (y p2 - y p1)
        dz2 = square (z p2 - z p1)
        square a = a * a

setDistances :: [Point] -> [Point]
setDistances pts = map setDistance pts
    where
        setDistance p = p { distances = ds }
            where
                ds = map (p `distance`) pts
scan :: [Coord] -> [Point] 
scan = (point (0,0,0):) . setDistances . map point

size :: [Point] -> Int
size = length . filter (not . is000)

readScanners :: [String] -> [Scanner]
readScanners = map (scan . (map toCoord)) . scannerGroups
    where 
        toCoord s = read ("("<>s<>")")
        scannerGroups = filter (isCoord . head) . groupBy (\s t -> isCoord s == isCoord t)  
        isCoord s = ',' `elem` s

rotate :: Rotation -> Coord -> Coord
rotate [[d,e,f],[g,h,i],[j,k,l]] (a,b,c) = (m,n,o)
    where
        m = a * d + b * g + c * j
        n = a * e + b * h + c * k
        o = a * f + b * i + c * l 

rotations :: Coord -> [Coord]
rotations c = map (flip rotate c) allRotations

altRotations :: [Rotation]
altRotations = [[[ 1, 0, 0] ,[ 0, 1, 0] ,[ 0, 0, 1]]
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

allRotations = [[[1, 0, 0], [0, 1, 0], [0, 0, 1]], [[ 0, -1,  0], [ 1,  0,  0], [ 0,  0,  1]], [[-1,  0,  0], [ 0, -1,  0], [ 0,  0,  1]], [[ 0,  1,  0], [-1,  0,  0], [ 0,  0,  1]], [[ 0,  0,  1], [ 0,  1,  0], [-1,  0,  0]], [[ 0, -1,  0], [ 0,  0,  1], [-1,  0,  0]], [[ 0,  0, -1], [ 0, -1,  0], [-1,  0,  0]], [[ 0,  1,  0], [ 0,  0, -1], [-1,  0,  0]], [[-1,  0,  0], [ 0,  1,  0], [ 0,  0, -1]], [[ 0, -1,  0], [-1,  0,  0], [ 0,  0, -1]], [[ 1,  0,  0], [ 0, -1,  0], [ 0,  0, -1]], [[ 0,  1,  0], [ 1,  0,  0], [ 0,  0, -1]], [[ 0,  0, -1], [ 0,  1,  0], [ 1,  0,  0]], [[ 0, -1,  0], [ 0,  0, -1], [ 1,  0,  0]], [[ 0,  0,  1], [ 0, -1,  0], [ 1,  0,  0]], [[0, 1, 0], [0, 0, 1], [1, 0, 0]], [[ 1,  0,  0], [ 0,  0, -1], [ 0,  1,  0]], [[0, 0, 1], [1, 0, 0], [0, 1, 0]], [[-1,  0,  0], [ 0,  0,  1], [ 0,  1,  0]], [[ 0,  0, -1], [-1,  0,  0], [ 0,  1,  0]], [[-1,  0,  0], [ 0,  0, -1], [ 0, -1,  0]], [[ 0,  0,  1], [-1,  0,  0], [ 0, -1,  0]], [[ 1,  0,  0], [ 0,  0,  1], [ 0, -1,  0]], [[ 0,  0, -1], [ 1,  0,  0], [ 0, -1,  0]]]

translation :: Coord -> Coord -> Coord
translation (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)

translate :: Coord -> Coord -> Coord
translate (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

intersection :: Scanner -> Scanner -> [(Coord,Coord)]
intersection ps qs = [(coord p, coord q) | p <- ps, q <- qs, (distances p `common` distances q) >= 11]

common :: (Eq a, Ord a) => [a] -> [a] -> Int
common as bs = common' 0 as bs
    where
        common' n [] _ = n
        common' n (a:as) bs | a `elem` bs = common' (n+1) as (bs \\ [a])
                            | otherwise =   common' n as bs

uniform :: (Eq a) => [a] -> Bool
uniform [] = True
uniform [a] = True
uniform (a:b:as) | a == b = uniform (b:as)
uniform _  = False

findPosition :: Scanner -> Scanner -> Maybe (Coord,Rotation)
findPosition ps qs = case intersection ps qs of
                       [] -> Nothing
                       pairs -> findPosition' pairs
    where 
        findPosition' :: [(Coord,Coord)] -> Maybe (Coord,Rotation)
        findPosition' pairs = head <$> find isHomogen (map (\r -> [(translation p (rotate r q),r)| (p,q) <- pairs]) allRotations)
        isHomogen :: [(Coord,Rotation)] -> Bool
        isHomogen = uniform

findPositions :: [Scanner] -> [((Int,Int),(Coord,Rotation))]
findPositions scs = result $ [((i,j),findPosition (scs!!i) (scs!!j)) | i <- [0..n-1], j <- [0..n-1], i /= j] 
    where 
        n = length scs
        result = map (\(a,b) -> (a,fromJust b)) . filter ((Nothing /=).snd)
        
setCoord :: Coord -> Point -> Point
setCoord (a,b,c) p = p { x = a, y = b, z = c }

acquire :: Scanner -> Scanner -> [Point]
acquire s t = case findPosition s t of
                Nothing -> []
                Just (tr,ro) -> nubBy (on (==) coord) (sortBy (comparing coord) (s <> (map (\p -> setCoord (convert (coord p)) p) t)))
                    where
                        convert = (translate tr) . (rotate ro)  

deleteAt :: [a] -> Int -> [a]
deleteAt [] _ = []
deleteAt (a:as) 0 = as
deleteAt (a:as) n = a:deleteAt as (n-1)

replaceAt :: [a] -> Int -> a -> [a]
replaceAt [] _ _ = []
replaceAt (a:as) 0 b = b:as
replaceAt (a:as) n b = a:replaceAt as (n-1) b 

mergeScanners :: [Scanner] -> Int -> Int -> [Scanner]
mergeScanners scs a b = deleteAt (replaceAt scs a (setDistances (acquire (scs!!a) (scs!!b)))) b

merge :: [Scanner] -> [Scanner] 
merge scs = case findPositions scs of
              [] -> scs
              (((a,b),_):_) -> mergeScanners scs a b

mergeAll :: [Scanner] -> Scanner
mergeAll [sc] = sc
mergeAll scs = mergeAll (merge scs)
         
mergeIO :: [Scanner] -> IO Scanner
mergeIO [sc] = return sc
mergeIO scs = do
    print (map length scs)
    mergeIO (merge scs)

manhattan :: Coord -> Integer
manhattan (x,y,z) = abs x + abs y + abs z

largestManhattan :: [Point] -> Integer
largestManhattan = maximum . map manhattan . translations . origins

origins :: Scanner -> [Coord]
origins = map coord . filter is000

translations :: [Coord] -> [Coord]
translations cds = [translation c d | c <- cds, d <- cds]
