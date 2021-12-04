module P04
    where

import Data.List ( find, transpose )

type Number = (Integer,Bool)
type Grid = [[Number]]
data Bingo = Bingo { numbers :: [Integer],
                     lastCalled :: Maybe Integer,
                     grids :: [Grid] }
     deriving (Eq,Show)

readBingo :: [String] -> Bingo
readBingo ss = Bingo nums Nothing grds
    where nums = read ("[" <> (head ss) <> "]")
          grds = readGrids (tail ss)
          readGrids :: [String] -> [Grid]
          readGrids [] = []
          readGrids ("":a:b:c:d:e:ss) = readGrid [a,b,c,d,e] : readGrids ss
          readGrid :: [String] -> Grid
          readGrid = map (map ((\n -> (n,False)) . read) . words)

draw :: Bingo -> Bingo
draw bingo = Bingo rest called grids'
    where
        (n:rest) = numbers bingo
        called = Just n
        grids' = map (\grid -> if not (solved grid) then markGrid n grid else grid) (grids bingo)

markGrid :: Integer -> Grid -> Grid
markGrid n grid = map (markLine n) grid
    where
        markLine :: Integer -> [Number] -> [Number]
        markLine n numbers = map (\(x,b) -> (x,(x==n) || b)) numbers

allSolved :: Bingo -> Bool
allSolved bingo = filter solved (grids bingo) == grids bingo

winner :: Bingo -> Maybe Grid
winner bingo = find solved (grids bingo)

solved :: Grid -> Bool
solved grid = (any ((==line) . map snd) grid) || (any ((==line) . map snd) (transpose grid))
    where
        line = [True,True,True,True,True]

solutionA :: Bingo -> Maybe Integer
solutionA bingo = (*) <$> a <*> b
    where
        a :: Maybe Integer
        a = winnerBingo bingo >>= lastCalled
        b = sumNonMarked <$> winnerGrid
        winnerGrid :: Maybe Grid
        winnerGrid = (winnerBingo bingo) >>= winner


winnerBingo :: Bingo -> Maybe Bingo
winnerBingo bingo = find (\b -> winner b /= Nothing) bingos
    where
        bingos = take (length (numbers bingo)) (iterate draw bingo)


sumNonMarked :: Grid -> Integer
sumNonMarked grid = sum (map fst (filter (not . snd) (concat grid)))
