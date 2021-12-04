module P04
    where

import Data.List ( find, transpose )

type Number = (Integer,Bool)
type Grid = [[Number]]
data Bingo = Bingo { numbers :: [Integer],
                     lastCalled :: Maybe Integer,
                     winners :: [(Int,Integer)],
                     grids :: [Grid] }
     deriving (Eq,Show)

readBingo :: [String] -> Bingo
readBingo ss = Bingo nums Nothing [] grds
    where nums = read ("[" <> (head ss) <> "]")
          grds = readGrids (tail ss)
          readGrids :: [String] -> [Grid]
          readGrids [] = []
          readGrids ("":a:b:c:d:e:ss) = readGrid [a,b,c,d,e] : readGrids ss
          readGrid :: [String] -> Grid
          readGrid = map (map ((\n -> (n,False)) . read) . words)

draw :: Bingo -> Bingo
draw bingo = Bingo numbers' lastCalled' winners' grids'
    where
        (n:numbers') = numbers bingo
        lastCalled' = Just n
        winners' = winners bingo <> newWinner
        newWinner = [ (i,n) | i <- [0..length grids' -1]
                    , solved (grids'!!i)
                    , not (i `elem` (map fst (winners bingo)))]
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

solutionA :: Bingo -> Integer
solutionA bingo = calledNumber * sumNonMarked winningGrid
    where
        lastBingo = last (take (length (numbers bingo)) (iterate draw bingo))
        (firstWinner,calledNumber) = head (winners lastBingo)
        winningGrid = (grids lastBingo) !! firstWinner
        
solutionB :: Bingo -> Integer
solutionB bingo = calledNumber * sumNonMarked winningGrid
    where
        lastBingo = last (take (length (numbers bingo)) (iterate draw bingo))
        (lastWinner,calledNumber) = last (winners lastBingo)
        winningGrid = (grids lastBingo) !! lastWinner

winnerBingo :: Bingo -> Maybe Bingo
winnerBingo bingo = find (\b -> winner b /= Nothing) bingos
    where
        bingos = take (length (numbers bingo)) (iterate draw bingo)


sumNonMarked :: Grid -> Integer
sumNonMarked grid = sum (map fst (filter (not . snd) (concat grid)))
