module P09
    where

import Data.Char
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

type Height = Int
type Coord = (Int,Int)
type HeightMap = M.Map Coord Height

heightMap :: [String] -> HeightMap
heightMap input = M.unions $
    [M.fromList [((row,col), digitToInt (input!!row!!col))
      | col <- [0..length (head input) - 1]
      , row <- [0..length input - 1]]]
   
level :: HeightMap -> Coord -> Height
level m (row,col) = case M.lookup (row,col) m of
                      Nothing -> 100
                      Just l -> l

adjacentCoords :: Coord -> [Coord]
adjacentCoords (row,col) = [(row,col-1),(row-1,col),(row,col+1),(row+1,col)]

adjacentLevels :: HeightMap -> Coord -> [Height]
adjacentLevels m coord = L.map (level m) (adjacentCoords coord)

lowest :: HeightMap -> Coord -> Height -> Maybe Height
lowest m coord height = case all higher (adjacentLevels m coord) of
                   True -> Just height
                   False -> Nothing
    where
        higher level = level > height

lowestCoord :: HeightMap -> Coord -> Height -> Maybe Coord
lowestCoord  m coord height = case all higher (adjacentLevels m coord) of
                                True -> Just coord
                                False -> Nothing
    where
        higher level = level > height

lowestCoords :: HeightMap -> [Coord]
lowestCoords m = catMaybes (L.map (\(coord,height) -> lowestCoord m coord height) (M.toList m))

lowerHeights :: HeightMap -> [Height]
lowerHeights m = catMaybes [lowest m coord height| (coord,height) <- M.toList m]

riskLevels :: [String] -> [Height]
riskLevels input = map (+1) (lowerHeights (heightMap input))

basin :: HeightMap -> Coord -> [Coord]
basin m coord = basin' m [] [coord]
    where
        basin' :: HeightMap -> [Coord] -> [Coord] -> [Coord]
        basin' m vs [] = vs
        basin' m vs (t:ts) | t `elem` vs = basin' m vs ts
        basin' m vs (t:ts) | otherwise = case (M.lookup t m) of
                               Nothing -> basin' m vs ts
                               Just h -> if h < 9 then basin' m (vs <> [t]) ((adjacentCoords t) <> ts) else basin' m vs ts

threeLargestBasinSizes :: HeightMap -> [Int]
threeLargestBasinSizes hm = L.take 3 (L.reverse (L.sort (L.map length basins)))
    where
        basins = L.map (basin hm) (lowestCoords hm)
