module P03
    where

import Data.List
import Data.Ord
import Data.Char

binary = map digitToInt

inverse n = 1 - n
mostCommon :: [Int] -> Int
mostCommon bits = if sum bits >= sum (map inverse bits) then 1 else 0
    where half = (length bits) `div` 2

mostCommonBitsPattern :: [[Int]] -> [Int]
mostCommonBitsPattern = map mostCommon . transpose

leastCommonBitsPattern :: [[Int]] -> [Int]
leastCommonBitsPattern = map (\n -> 1-n) . mostCommonBitsPattern

binaryToInt :: [Int] -> Int
binaryToInt = foldl (\acc b -> acc * 2 + b) 0

gammaRate :: [[Int]] -> Int
gammaRate = binaryToInt . mostCommonBitsPattern

epsilonRate :: [[Int]] -> Int
epsilonRate = binaryToInt . leastCommonBitsPattern

reduceByMostCommonBit :: Int -> [[Int]] -> [[Int]]
reduceByMostCommonBit _ [p] = [p]
reduceByMostCommonBit n patterns = filter (\pattern -> pattern!!n == mostCommon) patterns
    where
        mostCommon = (mostCommonBitsPattern patterns) !! n

reduceByMostCommonBits :: [[Int]] -> [Int]
reduceByMostCommonBits patterns = reduceByMostCommonBits' 0 patterns
    where
        reduceByMostCommonBits' :: Int -> [[Int]] -> [Int]
        reduceByMostCommonBits' _ [p] = p
        reduceByMostCommonBits' n patterns = reduceByMostCommonBits' (n+1) patterns'
            where
                patterns' = reduceByMostCommonBit n patterns

reduceByLeastCommonBit :: Int -> [[Int]] -> [[Int]]
reduceByLeastCommonBit _ [p] = [p]
reduceByLeastCommonBit n patterns = filter (\pattern -> pattern!!n == leastCommon) patterns
    where
        leastCommon = (leastCommonBitsPattern patterns) !! n

reduceByLeastCommonBits :: [[Int]] -> [Int]
reduceByLeastCommonBits patterns = reduceByLeastCommonBits' 0 patterns
    where
        reduceByLeastCommonBits' :: Int -> [[Int]] -> [Int]
        reduceByLeastCommonBits' _ [p] = p
        reduceByLeastCommonBits' n patterns = reduceByLeastCommonBits' (n+1) patterns'
            where
                patterns' = reduceByLeastCommonBit n patterns

oxygenGeneratorRating = binaryToInt . reduceByMostCommonBits

co2ScrubberRating = binaryToInt . reduceByLeastCommonBits
