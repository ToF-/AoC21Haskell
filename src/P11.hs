module P11
    where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

sample = ["5483143223"
         ,"2745854711"
         ,"5264556173"
         ,"6141336146"
         ,"6357385478"
         ,"4167524645"
         ,"2176841721"
         ,"6882881134"
         ,"4846848554"
         ,"5283751526"]
type Coord = (Int,Int)
type Energy = Char
type Grid = M.Map Coord Energy

step :: [[Char]] -> [[Char]]
step = toStrings .  evolve . fromStrings

fromStrings :: [[Char]] -> Grid
fromStrings = M.fromList . coords
    where
        coords ss = [((row,col),ss!!row!!col)
                    | row <- [0..length ss - 1]
                    , col <- [0..length (head ss) - 1]]

toStrings :: Grid -> [[Char]]
toStrings = map catMaybes . sdrooc
    where
        sdrooc g = [[M.lookup (row,col) g |col <- [0..maxCol]] |row <- [0..maxRow]]
            where
            (maxRow,maxCol) = fst (M.findMax g)

evolve :: Grid -> Grid
evolve = M.map release . flash . M.map increase
    where
    increase :: Energy -> Energy
    increase c | c `elem` "012345678" = succ c
    increase '9' = '*'
    increase c   = c

    flash :: Grid -> Grid
    flash g = if M.null (stars g) then g else ripple g (M.keys (stars g))

    stars :: Grid -> Grid
    stars = M.filter ((==)'*')

    ripple :: Grid -> [Coord] -> Grid
    ripple g coords = flash (L.foldl rippleCoord g coords)
        where
            rippleCoord :: Grid -> Coord -> Grid
            rippleCoord g (row,col) = M.adjust (const '.') (row,col) g'
                where
                    g' = L.foldl increaseCoord g (adjacents (row,col))
                    increaseCoord g (row,col) = M.adjust increase (row,col) g
                    adjacents (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),(i,j-1),(i,j+1),(i+1,j-1),(i+1,j),(i+1,j+1)]

    release :: Energy -> Energy
    release '.' = '0'
    release c   = c

countFlashes :: Int -> [[Char]] -> Int
countFlashes n =  sum . L.map (M.size . M.filter (=='0')) . steps n . fromStrings

steps :: Int -> Grid -> [Grid]
steps n = take (n+1) . iterate evolve

firstStepWithAllFlash :: [[Char]] -> Maybe Int
firstStepWithAllFlash = findFullFlash 0 . fromStrings
    where
        findFullFlash :: Int -> Grid -> Maybe Int
        findFullFlash 1000 _ = Nothing
        findFullFlash n g | (M.size (M.filter (=='0') g)) == (M.size g) = Just n
        findFullFlash n g | otherwise = findFullFlash (succ n) (evolve g)
