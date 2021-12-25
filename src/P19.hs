module P19
    where

import Data.List
import Data.Maybe

type Signature = [Integer]
type Scanner = [Point]
type Coords = (Integer,Integer,Integer)

data Point = Point { x :: Integer, y :: Integer, z :: Integer }
    deriving (Eq,Show,Ord)

type Rotation = [[Integer]]

data Position = Position { from :: Int
                         , to :: Int
                         , tr :: Coords
                         , rot :: Rotation }
    deriving (Eq,Show,Ord)

point :: Coords -> Point
point (a,b,c) = Point { x = a, y = b, z = c }

coords :: Point -> Coords
coords (Point a b c) = (a,b,c)

sq :: Integer -> Integer
sq a = a * a

distance :: Point -> Point -> Double 
distance p1 p2 = sqrt ( dx2 + dy2 + dz2 )
    where
        dx2 = fromIntegral (square ( x p2 - x p1 ))
        dy2 = fromIntegral (square ( y p2 - y p1 ))
        dz2 = fromIntegral (square ( z p2 - z p1 ))
        square a = a * a

signature :: Point -> [Point] -> [Integer]
signature pt = filter (>0) . sort . map (\p -> round ((pt `distance` p) * 10000)) 

report :: Scanner -> [(Point,Signature)]
report pts = map (\p -> (p, signature p pts)) pts

commonPoints :: Scanner -> Scanner -> Int -> [(Point,Point)]
commonPoints ps qs n = map (\(p,q,_) -> (p,q)) $ filter (\(_,_,s) -> length s >= n) $ [(p,q,common sp sq) | (p,sp) <- report ps, (q,sq) <- report qs]
    where
        common :: Signature -> Signature -> Signature
        common s t = filter (\d -> d `elem` t) s

translation :: Point -> Point -> (Integer, Integer, Integer)
translation (Point a b c) (Point d e f) = (a-d,b-e,c-f)

translate :: Point -> Coords -> Point
translate (Point a b c) (d,e,f) = Point (a+d) (b+e) (c+f)

translationT :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
translationT (a,b,c) (d,e,f) = (a-d,b-e,c-f)

rotate :: Point -> Rotation -> Point
rotate (Point a b c) [[d,e,f],[g,h,i],[j,k,l]] = Point m n o
    where
        m = a * d + b * g + c * j
        n = a * e + b * h + c * k
        o = a * f + b * i + c * l 

rotations = [[[ 1, 0, 0]
             ,[ 0, 1, 0]
             ,[ 0, 0, 1]]

            ,[[ 1, 0, 0]
             ,[ 0, 0,-1]
             ,[ 0, 1, 0]]

            ,[[ 1, 0, 0]
             ,[ 0,-1, 0]
             ,[ 0, 0,-1]]

            ,[[ 1, 0, 0]
             ,[ 0, 0, 1]
             ,[ 0,-1, 0]]

            ,[[ 0,-1, 0]
             ,[ 1, 0, 0]
             ,[ 0, 0, 1]]

            ,[[ 0, 0, 1]
             ,[ 1, 0, 0]
             ,[ 0, 1, 0]]

            ,[[ 0, 1, 0]
             ,[ 1, 0, 0]
             ,[ 0, 0,-1]]

            ,[[ 0, 0,-1]
             ,[ 1, 0, 0]
             ,[ 0,-1, 0]]

            ,[[ 1, 0, 0]
             ,[ 0,-1, 0]
             ,[ 0, 0, 1]]

            ,[[-1, 0, 0]
             ,[ 0, 0,-1]
             ,[ 0,-1, 0]]

            ,[[-1, 0, 0]
             ,[ 0, 1, 0]
             ,[ 0, 0,-1]]

            ,[[-1, 0, 0]
             ,[ 0, 0, 1]
             ,[ 0, 1, 0]]

            ,[[ 0, 1, 0]
             ,[-1, 0, 0]
             ,[ 0, 0, 1]]

            ,[[ 0, 0, 1]
             ,[-1, 0, 0]
             ,[ 0,-1, 0]]

            ,[[ 0,-1, 0]
             ,[-1, 0, 0]
             ,[ 0, 0,-1]]

            ,[[ 0, 0,-1]
             ,[-1, 0, 0]
             ,[ 0, 1, 0]]

            ,[[ 0, 0,-1]
             ,[ 0, 1, 0]
             ,[ 1, 0, 0]]

            ,[[ 0, 1, 0]
             ,[ 0, 0, 1]
             ,[ 1, 0, 0]]

            ,[[ 0, 0, 1]
             ,[ 0,-1, 0]
             ,[ 1, 0, 0]]

            ,[[ 0,-1, 0]
             ,[ 0, 0,-1]
             ,[ 1, 0, 0]]

            ,[[ 0, 0,-1]
             ,[ 0,-1, 0]
             ,[-1, 0, 0]]

            ,[[ 0,-1, 0]
             ,[ 0, 0, 1]
             ,[-1, 0, 0]]

            ,[[ 0, 0, 1]
             ,[ 0, 1, 0]
             ,[-1, 0, 0]]

            ,[[ 0, 1, 0]
             ,[ 0, 0,-1]
             ,[-1, 0, 0]]]



rotateCommonPoints :: [(Point,Point)] -> Rotation -> [(Point,Point)]
rotateCommonPoints cps r = map (\(p,q) -> (p,rotate q r)) cps

isHomogen :: ([(Point,Point)],Rotation) -> Bool
isHomogen = (1==) . length . group . sort . map (uncurry translation) . fst

findHomogen :: [(Point, Point)] -> Maybe (Coords,Rotation)
findHomogen cps = result <$> find isHomogen allRotatedCommonPoints
    where 
        allRotatedCommonPoints = map (\r -> (rotateCommonPoints cps r,r)) rotations
        result :: ([(Point,Point)],Rotation) -> (Coords, Rotation)
        result (cps,r) = (uncurry translation (head cps),r)

assemble :: Scanner -> Scanner -> Scanner
assemble sc0 sc1 = let
    cps = commonPoints sc0 sc1 11
                    in case (findHomogen cps) of
                         Nothing -> sc0
                         Just (t,r) -> nub (sort (sc0 <> sc1'))
                            where
                                sc1' = map ((flip translate t).(flip rotate r)) sc1

findPosition :: [Scanner] -> Int -> Int -> Maybe Position
findPosition scs a b = result <$> findHomogen (commonPoints (scs!!a) (scs!!b) 11)
    where
        result (t,r) = Position { from = a, to = b, tr = t, rot = r }

findAllPositions :: [Scanner] -> [Position]
findAllPositions scs = catMaybes $ [findPosition scs a b | a <- [0..n], b <- [0..n], a /= b]
    where n = length scs - 1

merge :: [Scanner] -> [[Point]]
merge scs = map (nub . sort . concatMap (\(a,b) -> assemble (scs!!a) (scs!!b))) $ mergeProgram (length scs) 

mergeAll :: [Scanner] -> [Point]
mergeAll scs | length scs == 1 = head scs
             | otherwise  = mergeAll (merge scs)

mergeProgram :: Int -> [[(Int,Int)]]
mergeProgram n = (groupBy (\a b  -> fst a == fst b) . sort . map (\[a,b] -> (a,b)) . filter ((==2).length) . subsequences) [0..n-1]

readScanners :: [String] -> [Scanner]
readScanners = map (map toPoint) . scannerGroups
    where toPoint s = point (read ("("<>s<>")"))

scannerGroups :: [String] -> [[String]]
scannerGroups = filter (isCoord . head) . groupBy (\s t -> isCoord s == isCoord t)  
    where  isCoord s = ',' `elem` s

