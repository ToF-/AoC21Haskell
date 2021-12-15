module P15
    where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Heap as H
import qualified Data.List as L
import Data.Maybe
import Data.Char

type Coord = (Int,Int) 
type Risk = Int
data Node = Node { risk :: Risk
                 , totalRisk :: Risk
                 , predecessor :: Maybe Coord }
    deriving (Eq, Show)

type VisitedSet = S.Set Coord
type RiskMap = M.Map Coord Node
type ToVisitHeap = H.MinPrioHeap Risk Coord

data Cave = Cave { maxX :: Int
                 , maxY :: Int
                 , risks :: RiskMap
                 , visited :: VisitedSet
                 , toVisit :: ToVisitHeap
                }
    deriving (Eq,Show)

infinite :: Risk 
infinite = 1000000

cave :: [String] -> Cave
cave ss =  Cave { maxX = mx
                , maxY = my
                , risks = M.adjust (const (Node 0 0 Nothing)) (0,0) $ M.fromList $ L.map (\coord -> (coord, Node (riskLevel coord) infinite Nothing)) allCoords
                , visited = S.insert (0,0) S.empty
                , toVisit = H.insert (0,(0,0)) H.empty }
        where 
            allCoords = [(x,y)| x<-[0..mx], y<-[0..my]]
            mx = length (head ss) - 1
            my = length ss - 1
            riskLevel (i,j) = digitToInt (ss!!i!!j)

neighbors :: Cave -> Coord -> [Coord]
neighbors c (x,y) = filter (\(i,j) -> i >= 0 
                                   && j >= 0 
                                   && i <= maxX c 
                                   && j <= maxY c 
                                   && not (S.member (i,j) (visited c))) 
                                   [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]


visit :: Cave -> Cave 
visit c | H.isEmpty (toVisit c) = c
        | otherwise =
            let
                ((level,(x,y)),toVisit') = fromJust $ H.view (toVisit c)
                coordNodes = L.map (\xy -> (xy,(fromJust (M.lookup xy (risks c))))) $ neighbors c (x,y)
                risks' = L.foldl adjustRisk (risks c) coordNodes
                adjustRisk riskMap ((i,j),(Node r t p)) = case t > level + r of
                                                    True -> M.adjust (const (Node r (level+r) (Just (x,y)))) (i,j) riskMap
                                                    False -> riskMap
                visited' = S.insert (x,y) (visited c)
                coordNodes' = L.map (\xy -> (xy,(fromJust (M.lookup xy risks')))) $ neighbors c (x,y)
                toVisit'' = H.fromList $ L.nub $ L.sort $ H.toList $ L.foldl insertToVisit toVisit' coordNodes'
                insertToVisit :: ToVisitHeap -> (Coord,Node) -> ToVisitHeap
                insertToVisit h ((i,j),(Node r t p)) = H.insert (t,(i,j)) h

             in c { toVisit = toVisit'',  risks = risks', visited = visited' }
                
        
visitAll :: Cave -> Cave
visitAll c | H.isEmpty (toVisit c) = c
           | otherwise = visitAll (visit c)

aSample = ["1163751742"
          ,"1381373672"
          ,"2136511328"
          ,"3694931569"
          ,"7463417111"
          ,"1319128137"
          ,"1359912421"
          ,"3125421639"
          ,"1293138521"
          ,"2311944581"]

increase :: String -> String
increase s = map (\c -> if c == '9' then '1' else succ c) s

extended :: [String] -> [String]
extended ss = let
                ext1 = map increase ss
                ext2 = map increase ext1
                ext3 = map increase ext2
                ext4 = map increase ext3
                join :: [String] -> [String] -> [String]
                join as bs = zipWith (<>) as bs
                extrow = ss `join` ext1 `join` ext2 `join` ext3 `join` ext4  
                extrow1 = map increase extrow
                extrow2 = map increase extrow1
                extrow3 = map increase extrow2
                extrow4 = map increase extrow3
              in extrow <> extrow1 <> extrow2 <> extrow3 <> extrow4
