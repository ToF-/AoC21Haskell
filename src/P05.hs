module P05
    where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Char

type Point = (Integer,Integer)
type Line = (Point,Point)
type Plane = M.Map Point Integer

overlapsHV :: [Line] -> [(Point,Integer)]
overlapsHV = L.sort . M.toList . M.filter (>=2) . L.foldl mapLineHV M.empty

overlapsHVD :: [Line] -> [(Point,Integer)]
overlapsHVD = L.sort . M.toList . M.filter (>=2) . L.foldl mapLineHVD M.empty

mapLineHV :: Plane -> Line -> Plane
mapLineHV p ((x1,y1),(x2,y2)) | x1 == x2 && y1 > y2 = mapLineHV p ((x2,y2),(x1,y1))
mapLineHV p ((x1,y1),(x2,y2)) | x1 == x2            = mapPoints p [(x1,y) | y <- [y1..y2]]
mapLineHV p ((x1,y1),(x2,y2)) | y1 == y2 && x1 > x2 = mapLineHV p ((x2,y2),(x1,y1))
mapLineHV p ((x1,y1),(x2,y2)) | y1 == y2            = mapPoints p [(x,y1) | x <- [x1..x2]]
mapLineHV p _ = p

mapLineHVD :: Plane -> Line -> Plane
mapLineHVD p ((x1,y1),(x2,y2)) | x1 == x2 || y1 == y2 = mapLineHV p ((x1,y1),(x2,y2))
mapLineHVD p ((x1,y1),(x2,y2)) | x1 < x2 && y1 < y2 = mapPoints p (zip [x1..x2] [y1..y2])
mapLineHVD p ((x1,y1),(x2,y2)) | x1 < x2 && y1 > y2 = mapPoints p (zip [x1..x2] (reverse [y2..y1]))
mapLineHVD p ((x1,y1),(x2,y2)) | x1 > x2 && y1 < y2 = mapPoints p (zip (reverse [x2..x1]) [y1..y2])
mapLineHVD p ((x1,y1),(x2,y2)) | x1 > x2 && y1 > y2 = mapPoints p (zip (reverse [x2..x1]) (reverse [y2..y1]))

mapLineHVD p l = error "wrong wrong wrong"

mapPoints :: Plane -> [Point] -> Plane
mapPoints p pts = L.foldl mapPoint p pts

mapPoint :: Plane -> Point -> Plane
mapPoint p pt = M.insertWith (+) pt 1 p

allPointsHV :: [Line] -> [(Point,Integer)]
allPointsHV = L.sort . M.toList . L.foldl mapLineHV M.empty

allPointsHVD :: [Line] -> [(Point,Integer)]
allPointsHVD = L.sort . M.toList . plane

plane :: [Line] -> Plane
plane = L.foldl mapLineHVD M.empty

display :: Plane -> [String]
display p = [ displayLine y | y <- [minY..maxY]]
    where
        displayLine j = [displayPoint (i,j) | i <- [minX..maxX]]
        displayPoint (col,row) = case M.lookup (col,row) p of
                                     Nothing -> '.'
                                     Just v  -> intToDigit (fromIntegral v)
        minX = minimum (L.map (fst . fst) (M.toList p))
        maxX = maximum (L.map (fst . fst) (M.toList p))
        minY = minimum (L.map (snd . fst) (M.toList p))
        maxY = maximum (L.map (snd . fst) (M.toList p))


