module P17
    where

import Data.List

data Probe = Probe { x :: Integer
                   , y :: Integer
                   , dx :: Integer
                   , dy :: Integer }
    deriving (Eq, Show)
type Range = (Integer, Integer)
        
step :: Probe -> Probe
step p = Probe { x = x p + dx p
               , y = y p + dy p
               , dx = drag (dx p)
               , dy = pred (dy p) }


drag :: Integer -> Integer
drag x = x - sign x
    where sign 0 = 0
          sign x = x `div` (abs x)


findWithinTarget :: Range -> Range  -> Probe -> Maybe Probe
findWithinTarget xr yr p = find (\p -> x p `within` xr && y p `within` yr) (take 500 (iterate step p))

within n (a,b) = n >= a && n <= b

findMaxY :: Range -> Range -> Probe -> Maybe Integer
findMaxY xr yr p | findWithinTarget xr yr p /= Nothing = Just $ maximum (map y (takeWhile (\p -> not (x p `within` xr && y p `within` yr)) (iterate step p)))
                 | otherwise = Nothing

findVelocityWithMaxY :: Range -> Range -> Maybe Integer
findVelocityWithMaxY (x0,x1) (y0,y1) = let
    dxs = [0..x0*2]
    dys = [0..negate y0 *2]
    trials = [findMaxY (x0,x1) (y0,y1) (Probe 0 0 dx dy) | dx <- dxs, dy <- dys]
                                        in maximum trials

initialVelocities :: Range -> Range -> [Range]
initialVelocities (x0,x1) (y0,y1) = let
    dxs = [0..x0*2]
    dys = [y0..negate y0*2]
                                     in [(dx,dy) | dx <- dxs, dy <- dys, (findWithinTarget (x0,x1) (y0,y1) (Probe 0 0 dx dy)) /= Nothing]
