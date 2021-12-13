module P12B
    where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char

data Cave = Small String
          | Big String
    deriving (Eq,Show,Ord)

type Graph = M.Map Cave [Cave]
type Connection = (Cave,Cave)
type Path = [Cave]

cave :: String -> Cave
cave s | isUpper (head s) = Big s
       | otherwise        = Small s

graph :: [String] -> Graph
graph ss = L.foldl addNode M.empty $ connections <> flipConnections
    where
        connections = L.map connection ss
        flipConnections = L.map (\(a,b) -> (b,a)) connections

        connection :: String -> Connection
        connection s = (start,end)
            where
                start = cave (takeWhile (/= '-') s)
                end   = cave (tail (dropWhile (/= '-') s))

        addNode :: Graph -> Connection -> Graph
        addNode g (start,end) = M.insertWith (flip (<>)) start [end] g

continue :: Graph -> Path -> [Path]
continue g [] = error "empty path given to continue"
continue g (Small "end":_) = []
continue g p = case M.lookup (head p) g of
                 Nothing -> []
                 Just cs -> L.map (\c -> c:p) (L.filter visitable cs)
                    where
                        visitable (Big _) = True
                        visitable (Small "start") = False
                        visitable (Small c) | visitedTwice p == Nothing = True
                        visitable (Small c) | visitedTwice p == Just (Small c) = False
                        visitable (Small c) | visitedTwice p /= Nothing = not ((Small c) `elem` p)

visitedTwice :: Path -> Maybe Cave
visitedTwice p = fmap head $ L.find (\g -> length g == 2) $ L.group $ L.sort $ L.filter isSmall p    
    where
        isSmall (Small _) = True
        isSmall (Big _) = False


continuePaths :: Graph -> [Path] -> [Path]
continuePaths g ps = L.concatMap (continue g) ps 

allPaths :: Graph -> [Path] -> [Path]
allPaths g ps = converge $ iterate (continuePaths g) ps 
    where
        converge :: [[Path]] -> [Path]
        converge [] = []
        converge ([]:_) = []
        converge (ps:pss) = ps <> converge pss
    
solutions :: Graph -> [Path]
solutions g = L.map reverse $ L.filter (\p -> head p == Small "end") $ allPaths g [[Small "start"]]

