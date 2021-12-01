module P01
    where

import Data.List

increases depths = length (filter id (zipWith (>) (tail depths) depths))

sums (a:b:c:xs) = (a+b+c:sums (b:c:xs))
sums xs = [sum xs]

windowIncreases = increases . sums


