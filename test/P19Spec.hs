module P19Spec
    where

import Test.Hspec
import P19
import Sample19Data 
import Data.List

spec :: SpecWith ()
spec = do
    describe "distance" $ do
        it "is determined by two points positions" $ do
            let p1 = point (2, 3, 1)
            let p2 = point (8,-5, 0)
            distance p1 p2 `shouldBe` 101 

    describe "scan" $ do
        it "tell the distances from a point to every other point in a scanner range" $ do
            let scanner = scan [(2,3,1), (8,-5,0),(4,2,-10)]
            scanner `shouldBe`
                [Point {x = 2, y = 3, z = 1, distances = [101,126]}
                ,Point {x = 8, y = -5, z = 0, distances = [101,165]}
                ,Point {x = 4, y = 2, z = -10, distances = [126,165]}]

    describe "readScanners" $ do
        it "read scanners from a file" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            head (head sample) `shouldBe` 
                Point {x = 404, y = -588, z = -901, distances = [19429,21242,1004246,1136955,1146209,1154534,1165776,1178580,1292485,1712390,1734501,1795355,2277243,2281254,2401186,3214416,3385779,3759893,3808714,3827748,4390731,5428385,5605830,5937465]}
            last (last sample) `shouldBe` 
                Point {x = 30, y = -46, z = -14, distances = [37469,577669,670424,713284,763982,800379,833333,943704,991242,1004955,1015017,1091606,1125425,1156030,1177106,1184870,1211276,1224629,1277230,1294605,1311389,1313990,1374678,1395301,1445358]}
