module P06
    where

generation :: [Integer] -> [Integer]
generation g = map decrease g <> [8 | x <- g, x == 0]
    where
        decrease 0 = 6
        decrease x = pred x

nthGeneration :: [Integer] -> Int -> [Integer]
nthGeneration g i = (iterate generation g) !! i
