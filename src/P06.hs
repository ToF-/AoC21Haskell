module P06
    where

generation :: [Integer] -> [Integer]
generation g = map decrease g <> [8 | x <- g, x == 0]
    where
        decrease 0 = 6
        decrease x = pred x

nthGeneration :: [Integer] -> Int -> [Integer]
nthGeneration g i = (iterate generation g) !! i

tally :: [Integer] -> [Int]
tally g = map count [0..8]
    where
        count x = length (filter (==x) g)  


evolve :: [Int] -> [Int]
evolve [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]

nthEvolution :: [Int] -> Int -> [Int]
nthEvolution t i = (iterate evolve t) !! i
