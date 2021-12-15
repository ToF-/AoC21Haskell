module P14
    where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Ord

type Element = Char
type Template = [Char]
type Pair = [Char]
type Rules = [(Pair,Element)]
type Counter = M.Map Template Int



counter :: Pair -> Counter -> Int
counter p c = case M.lookup p c of
                Nothing -> 0
                Just n -> n

sampleRules = [("CH",'B')
              ,("HH",'N')
              ,("CB",'H')
              ,("NH",'C')
              ,("HB",'C')
              ,("HC",'B')
              ,("HN",'C')
              ,("NN",'C')
              ,("BH",'H')
              ,("NC",'B')
              ,("NB",'B')
              ,("BN",'B')
              ,("BB",'N')
              ,("BC",'B')
              ,("CC",'N')
              ,("CN",'C')]

pairs :: Template -> [Pair]
pairs [] = []
pairs [x] = []
pairs (x:x':xs) = [x,x'] : pairs (x':xs)

count :: (Eq a, Ord a) => [a] -> [(a,Int)]
count t = L.map (\g-> (head g, L.length g)) $ L.group $ L.sort t

countElements :: Template -> [(Template,Int)]
countElements = map (\(c,n) -> ([c],n)) . count

countPairs :: Template -> [(Template,Int)]
countPairs = count . pairs 


add :: Template -> Int -> Counter -> Counter
add t n c = M.insertWith (+) t n c

initialCounter :: Template -> Counter
initialCounter t = initialCounter' t M.empty
    where
        initialCounter' [] c = c
        initialCounter' [x] c = add [x] 1 c   
        initialCounter' (x:y:xs) c = add [x,y] 1 (add [y] (-1) (initialCounter' (y:xs) c))

stepCounter :: Rules -> Counter -> Counter
stepCounter rs c = L.foldl step M.empty (M.assocs c)
    where
        step :: Counter -> (Template,Int) -> Counter
        step c ([x],n) = add [x] n c
        step c ([x,y],n) = case L.lookup [x,y] rs of
                             Nothing -> add [x,y] n c
                             Just z -> add [x,z] n (add [z,y] n (add [z] (-n) c))

separate :: Counter -> M.Map Char Int
separate c = L.foldl separateElements M.empty (M.assocs c)
    where
        separateElements c ([x],n) = add x n c
        separateElements c ([x,y],n) = add x n (add y n c)
        add = M.insertWith (+)

afterStepCounter :: Template -> Rules -> Int -> Counter
afterStepCounter t rs n = L.last $ L.take (succ n) $ iterate (stepCounter rs) (initialCounter t)

step :: Rules -> Template -> Template
step _ [] = []
step _ [x] = [x]
step rs (x:y:xs) = case [x,y] `lookup` rs of
                      Nothing -> (x:  step rs (y:xs))
                      Just n  -> (x:n:step rs (y:xs))

afterStep :: Template -> Rules -> Int -> Template
afterStep t rs n = L.last $ L.take (succ n) (iterate (step rs) t) 

leastAndMostCommon :: Template -> ((Int,Element),(Int,Element))
leastAndMostCommon t = (L.minimum ts, L.maximum ts) 
    where ts = L.sort $ L.map (\g -> (L.length g, L.head g)) $ L.group $ L.sort t

leastAndMostCommonCounter :: Counter -> ((Int,Char),(Int,Char))
leastAndMostCommonCounter c = (L.minimum ns, L.maximum ns)
    where ns = L.sort $ L.map (\(x,n) -> (n,x)) $ L.filter (\(_,n) -> n > 0) $ M.toList (separate c)
