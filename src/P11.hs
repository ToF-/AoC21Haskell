module P11
    where

import qualified Data.Map as M
import Data.Maybe

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
evolve = M.map increase
    where
        increase '9' = '0'
        increase c = succ c
