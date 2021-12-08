module P07
    where

minimumFuel :: [Integer] -> Integer
minimumFuel ps = minimum (map fuel [minX..maxX])
    where
        minX = minimum ps
        maxX = maximum ps
        fuel x = sum (map (\n -> abs (x-n)) ps)

minimumFuel' :: [Integer] -> Integer
minimumFuel' ps = minimum (map fuel [minX..maxX])
    where
        minX = minimum ps
        maxX = maximum ps
        fuel x = sum (map (\n -> consumption (abs (x-n))) ps)

        consumption 0 = 0
        consumption x = (x * (x+1)) `div` 2
