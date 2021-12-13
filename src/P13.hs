module P13
    where

import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe

type Coord = (Integer,Integer)
type Page = S.Set Coord

page :: [String] -> Page
page = S.fromList . catMaybes . L.map coord


coord :: String -> Maybe Coord
coord "" = Nothing
coord s | startWith "fold along" s = Nothing
coord s = Just (row,col)
    where row = read (takeWhile (/= ',') s)
          col = read (tail (dropWhile (/= ',') s))


startWith :: String -> String -> Bool
startWith p s = take l s == p
    where l = length p
          

foldY :: Integer -> Page -> Page 
foldY y p = h `S.union` l
    where
        h = S.filter (\(i,j) -> j < y) p
        l = S.map (\(i,j) -> (i,y-j+y)) $ S.filter (\(i,j) -> j > y) p

foldX :: Integer -> Page -> Page
foldX x p = l `S.union` r
    where
        l = S.filter (\(i,j) -> i < x) p
        r = S.map (\(i,j) -> (x-i+x,j)) $ S.filter (\(i,j) -> i > x) p

execute :: [String] -> Page
execute ss = L.foldl (\p f -> f p) initial fs
    where
        initial = page ss
        fs = instructions ss

executeFirst :: [String] -> Page
executeFirst ss = (head fs) initial
    where
        initial = page ss
        fs = instructions ss

instructionLength = length "fold along _=" 

instructions :: [String] -> [Page -> Page]
instructions = catMaybes . L.map instruction
    where
        instruction s | startWith "fold along x=" s = Just $ foldX (read (drop instructionLength s))
        instruction s | startWith "fold along y=" s = Just $ foldY (read (drop instructionLength s))
        instruction _ = Nothing

solutionA :: [String] -> Int
solutionA = S.size . executeFirst 

display :: Page -> [String]
display p = [ [if S.member (x,y) p then '#' else '.' | x <- [minX p..maxX p]]
            | y <- [minY p..maxY p]]
    where
        minX = L.minimum . L.map fst . S.toList
        minY = L.minimum . L.map snd . S.toList
        maxX = L.maximum . L.map fst . S.toList
        maxY = L.maximum . L.map snd . S.toList
