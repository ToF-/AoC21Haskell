module P14
    where

import Data.List
type Element = Char
type Template = [Char]
type Pair = [Char]
type Rules = [(Pair,Element)]

step :: Rules -> Template -> Template
step _ [] = []
step _ [x] = [x]
step rs (x:y:xs) = case [x,y] `lookup` rs of
                      Nothing -> (x:  step rs (y:xs))
                      Just n  -> (x:n:step rs (y:xs))

afterStep :: Template -> Rules -> Int -> Template
afterStep t rs n = last $ take (succ n) (iterate (step rs) t) 

leastAndMostCommon :: Template -> ((Int,Element),(Int,Element))
leastAndMostCommon t = (minimum ts, maximum ts) 
    where ts = sort $ map (\g -> (length g, head g)) $ group $ sort t
