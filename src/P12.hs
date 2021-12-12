module P12
    where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char

data Cave = Small String
          | Big String
    deriving (Eq,Show,Ord)

type Graph = M.Map Cave [Cave]
type Connection = (Cave,Cave)
type Path = ([Cave],[Cave])

cave :: String -> Cave
cave s | isUpper (head s) = Big s
       | otherwise        = Small s

graph :: [String] -> Graph
graph ss = L.foldl addNode M.empty (L.map connection ss)
    where
        connection :: String -> Connection
        connection s = (start,end)
            where
                start = cave (takeWhile (/= '-') s)
                end   = cave (tail (dropWhile (/= '-') s))

        addNode :: Graph -> Connection -> Graph
        addNode g (start,end) = M.insertWith (flip (<>)) start [end] g

adjacents :: Graph -> Cave -> [Cave] -> [Cave]
adjacents g c vs = case M.lookup c g of
                     Nothing -> error "wrong initial cave"
                     Just cs -> L.filter (not . (`elem` vs)) cs

