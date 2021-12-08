module P08 where


import Data.List
import Data.Char
import Data.Maybe

sum1478 :: [String] -> Int
sum1478 ls = length (concatMap (\vs -> filter (\ v -> length v `elem` [2,4,3,7]) vs) vvs)
    where
        vvs :: [[String]]
        vvs = map (words . outputValues) ls
        outputValues l = tail (dropWhile (/='|') l)

type Segment = Char
type DigitModel = [Segment]
type Permutation = [Segment]
type Digit = ([Char],Integer)
type Entry = [String]

normalDigits :: [Digit]
normalDigits = [("abcefg",0)
               ,("cf",1)
               ,("acdeg",2)
               ,("acdfg",3)
               ,("bcdf",4)
               ,("abdfg",5)
               ,("abdefg",6)
               ,("acf",7)
               ,("abcdefg",8)
               ,("abcdfg",9)]

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....
-- 
--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
--

swapDigitModel :: DigitModel -> Permutation -> DigitModel
swapDigitModel d p = [p!!i | i <- map (\c -> ord c - (ord 'a')) d]

permutedDigit :: Permutation -> Digit -> Digit
permutedDigit p (dm,v) = (swapDigitModel dm p,v)

permutedDigits :: Permutation -> [Digit]
permutedDigits p = map (permutedDigit p) normalDigits

isPermutedWith :: Entry -> Permutation -> Bool
isPermutedWith e p = all (/= Nothing) (e `permuteWith` p)

permuteWith :: Entry -> Permutation -> [Maybe Integer]
permuteWith e p = map (`lookup` pds) e
    where
        pds = map (\(s,v) -> (sort s,v)) (permutedDigits p)

value :: Entry -> Permutation -> Integer
value e p = foldl convert 0 (catMaybes (e `permuteWith` p))
    where
        convert acc d = acc * 10 + d

findValue :: String -> Integer
findValue s = case find (\p -> e `isPermutedWith` p) allPermutations of
                Just p -> value r p
                Nothing -> error "permutation not found"
    where
        e = map sort (take 10 (words s))
        r = map sort (drop 11 (words s))
    
allPermutations :: [DigitModel]
allPermutations = permutations "abcdefg"

solve :: [String] -> Integer
solve ss = sum (map findValue ss)

