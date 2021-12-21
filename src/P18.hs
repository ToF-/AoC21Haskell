module P18
    where
import Data.Char
import Data.Maybe

type Index = Int
data SN = Pair Index (SN, SN)
        | Number Index Int
    deriving (Eq, Show)

sn :: String -> SN
sn = fst . parseSN 1

parseChar :: Char -> String -> String
parseChar _ "" = ""
parseChar p (c:s) | c == p = s
                  | otherwise = error "incorrect SN"
                  
parseSN :: Index -> String -> (SN, String)
parseSN i ('[':s) = parsePair i s
parseSN i (c:s) | isDigit c = parseNumber i (c:s)

parsePair :: Index -> String -> (SN, String)
parsePair i s = let
    (a, rest1) = parseSN (i*2) s
    rest2 = parseChar ',' rest1
    (b, rest3) = parseSN (i*2+1) rest2
    rest4 = parseChar ']' rest3
        in (Pair i (a,b), rest4)

parseNumber :: Index -> String -> (SN, String)
parseNumber i s = let
    num = read (takeWhile isDigit s)
    rest = dropWhile isDigit s
                   in (Number i num, rest)

ns :: SN -> String
ns (Number _ v) = show v
ns (Pair _ (a,b)) = "[" <> ns a <> "," <> ns b <> "]" 

numberIndices :: SN -> [Index]
numberIndices (Number i _) = [i]
numberIndices (Pair _ (a,b)) = numberIndices a <> numberIndices b 

nextLeftIx :: Index -> SN -> Maybe Index
nextLeftIx i sn = nextRightIx' i (reverse (numberIndices sn))

nextRightIx :: Index -> SN -> Maybe Index
nextRightIx i sn = nextRightIx' i (numberIndices sn)

nextRightIx' :: Index -> [Index] -> Maybe Index
nextRightIx' _ [] = Nothing
nextRightIx' _ [x] = Nothing
nextRightIx' k (i:is) | k == i = Just (head is)
                      | otherwise = nextRightIx' k is


replaceIx :: SN -> Index -> Int -> SN
replaceIx (Pair l (a,b)) i v = Pair l (replaceIx a i v, replaceIx b i v)
replaceIx (Number k v) i w | i == k = Number k w
                           | otherwise = Number k v 

addIx :: SN -> Index -> Int -> SN
addIx (Pair l (a,b)) i v = Pair l (addIx a i v, addIx b i v)
addIx (Number k v) i w | i == k = Number k (v+w)
                       | otherwise = Number k v
deleteIx :: SN -> Index -> SN
deleteIx (Pair h (Number i a, Number j b)) t | t == i = Number h b
deleteIx (Pair h (Number i a, Number j b)) t | t == j = Number h a
deleteIx (Number i a) _ = Number i a
deleteIx (Pair h (a,b)) t = Pair h (deleteIx a t, deleteIx b t)



nested :: SN -> Maybe SN
nested = nested' 0 
    where nested' _ (Number l n) = Nothing
          nested' n p@(Pair _ (Number _ _, Number _ _)) | n > 3 = Just p
                                                       | otherwise = Nothing
          nested' n (Pair l (a,b)) = case (nested' (n + 1) a) of
                                                Nothing -> nested' (n + 1) b
                                                Just p  -> Just p



explodePairIx :: SN -> SN -> SN
explodePairIx (Pair l (Number m a,Number n b)) sn = case (nextLeftIx m sn, nextRightIx n sn) of
                                    (Nothing,Nothing) -> error "nested pair not surrounded by numbers"
                                    (Nothing,Just j) -> deleteIx (addIx (replaceIx sn m 0) j b) n
                                    (Just i,Nothing) -> deleteIx (replaceIx (addIx sn i a) n 0) m 
                                    (Just i,Just j) -> deleteIx (replaceIx (addIx (addIx sn i a) j b) n 0) m


explodeOne :: SN -> SN
explodeOne sn = case nested sn of
               Nothing -> sn
               Just p -> explodePairIx p sn

explode :: SN -> SN
explode sn = case explodeOne sn == sn of
                  True -> sn
                  False -> explode (explodeOne sn)

hasSplitted :: SN -> Maybe SN
hasSplitted (Number i v) | v >= 10   = Just (Pair i (Number (i*2) (v `div` 2), Number (i*2+1) (v `div` 2 + v `mod` 2)))
                    | otherwise = Nothing
hasSplitted (Pair i (a, b)) = case hasSplitted a of
                           Nothing -> case hasSplitted b of
                                        Nothing -> Nothing
                                        Just s ->  Just (Pair i (a,s))
                           Just s -> Just (Pair i (s,b))

split :: SN -> SN
split sn = case hasSplitted sn of
             Nothing -> sn
             Just n -> n

reduce :: SN -> SN
reduce sn = case split (explode sn) == sn of
              True -> sn
              False -> reduce (split (explode sn))

renum :: SN -> Index -> SN
renum (Number _ v) i = Number i v
renum (Pair _ (a, b)) i = Pair i (renum a (i*2), renum b (i*2+1))

add :: SN -> SN -> SN
add a b = reduce (Pair 1 (renum a 2, renum b 3))

addAll :: [SN] -> SN
addAll = foldl1 add

magnitude :: SN -> Int
magnitude (Number _ v) = v
magnitude (Pair _ (a, b)) = 3 * (magnitude a) + 2 * (magnitude b)

largest :: [SN] -> Int
largest sns = maximum [max (magnitude (a `add` b)) (magnitude (b `add` a)) | a <- sns, b <- sns, a /= b]
