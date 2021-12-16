module P16
    where

import Data.Char

type Binary = String
data LengthMode = TotalLength Int
                | NbSubPackets Int
    deriving (Eq, Show)

data Version = Version Int
    deriving (Eq, Show)

data Value = Value Int Int
    deriving (Eq, Show)

data Packet = LiteralValue Version Value
            | Operator Version LengthMode [Packet]
    deriving (Eq, Show) 


hexToBinary :: String -> Binary
hexToBinary = concatMap hexToBin
    where
        hexToBin '0' = "0000"
        hexToBin '1' = "0001"
        hexToBin '2' = "0010"
        hexToBin '3' = "0011"
        hexToBin '4' = "0100"
        hexToBin '5' = "0101"
        hexToBin '6' = "0110"
        hexToBin '7' = "0111"
        hexToBin '8' = "1000"
        hexToBin '9' = "1001"
        hexToBin 'A' = "1010"
        hexToBin 'B' = "1011"
        hexToBin 'C' = "1100"
        hexToBin 'D' = "1101"
        hexToBin 'E' = "1110"
        hexToBin 'F' = "1111"

fromBinary :: Binary -> Int
fromBinary = foldl convert 0
    where
        convert :: Int -> Char -> Int
        convert acc bit = acc * 2 + digitToInt bit

version :: Binary -> Version 
version = Version . fromBinary . take 3

typeId :: Binary -> Int
typeId = fromBinary . take 3 . drop 3

value :: Binary -> Value
value = litValue (Value 6 0) . drop 6
    where
        litValue :: Value -> Binary -> Value
        litValue (Value s acc) ('1':a:b:c:d:bits) = litValue (Value (s+5) (acc * 16 + fromBinary [a,b,c,d])) bits
        litValue (Value s acc) ('0':a:b:c:d:bits) = Value (s+5) (acc * 16 + fromBinary [a,b,c,d])
        litValue v _ = error "incorrect literal value"

lengthMode :: Binary -> LengthMode
lengthMode = lenMode . drop 6
    where
        lenMode :: Binary -> LengthMode
        lenMode ('0':bits) = TotalLength (fromBinary (take 15 bits)) 
        lenMode ('1':bits) = NbSubPackets (fromBinary (take 11 bits))
        lenMode b = error $ "this is not a correct binary value:" <> b

packetLength :: Packet -> Int
packetLength (LiteralValue _ (Value n _)) = n
packetLength (Operator _ (TotalLength n) _) = 22 + n
packetLength (Operator _ (NbSubPackets n) ps) = 18 + sum (map packetLength ps)

packet :: Binary -> Packet
packet b = case typeId b of
             4 -> LiteralValue (version b) (value b)
             _ -> case (lengthMode b) of
                    (TotalLength n) -> Operator (version b) (TotalLength n) (packetsForLength n (drop 22 b))
                    (NbSubPackets n) -> Operator (version b) (NbSubPackets n) (packetsForCount n (drop 18 b))

packetsForLength :: Int -> Binary -> [Packet]
packetsForLength 0 _ = []
packetsForLength n b = p : packetsForLength (n-l) (drop l b) 
    where
        l = packetLength p
        p = packet b

packetsForCount :: Int -> Binary -> [Packet]
packetsForCount 0 _ = []
packetsForCount n b = p : packetsForCount (n-1) (drop l b)
    where
        l = packetLength p
        p = packet b

versionSum :: Packet -> Int
versionSum (LiteralValue (Version n) _) = n
versionSum (Operator (Version n) _ ps) = n + sum (map versionSum ps)
