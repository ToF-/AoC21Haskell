module P19Spec
    where

import Test.Hspec
import P19
import Sample19Data 
import Data.List as L
import qualified Data.Set as S

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
                [Point {x = 2, y = 3, z = 1, distances = S.fromList [0,101,126]}
                ,Point {x = 8, y = -5, z = 0, distances = S.fromList [0,101,165]}
                ,Point {x = 4, y = 2, z = -10, distances = S.fromList [0,126,165]}]

    describe "readScanners" $ do
        it "read scanners from a file" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            head (head sample) `shouldBe` 
                Point {x = 404, y = -588, z = -901, distances = S.fromList [0,19429,21242,1004246,1136955,1146209,1154534,1165776,1178580,1292485,1712390,1734501,1795355,2277243,2281254,2401186,3214416,3385779,3759893,3808714,3827748,4390731,5428385,5605830,5937465]}
            last (last sample) `shouldBe` 
                Point {x = 30, y = -46, z = -14, distances = S.fromList [0,37469,577669,670424,713284,763982,800379,833333,943704,991242,1004955,1015017,1091606,1125425,1156030,1177106,1184870,1211276,1224629,1277230,1294605,1311389,1313990,1374678,1395301,1445358]}

    describe "rotations" $ do
        it "tell all rotations for a coord" $ do
            let (x,y,z) = (10,20,30)
            rotations (x,y,z) `shouldBe`
                [(x,y,z),(x,z,-y),(x,-y,-z),(x,-z,y),(y,-x,z),(y,z,x),(y,x,-z),(y,-z,-x),(x,-y,z),(-x,-z,-y),(-x,y,-z),(-x,z,y),(-y,x,z),(-y,-z,x),(-y,-x,-z),(-y,z,-x),(z,y,-x),(z,x,y),(z,-y,x),(z,-x,-y),(-z,-y,-x),(-z,-x,y),(-z,y,x),(-z,x,-y)]

    describe "intersection" $ do
        it "tell coords of points that are common between two scanners" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            ((sample!!0) `intersection` (sample!!1)) `shouldBe`
                [((404,-588,-901),(-336,658,858)),((528,-643,409),(-460,603,-452)),((390,-675,-793),(-322,571,750)),((-537,-823,-458),(605,423,415)),((-485,-357,347),(553,889,-390)),((-345,-311,381),(413,935,-424)),((-661,-816,-575),(729,430,532)),((-618,-824,-621),(686,422,578)),((-447,-329,318),(515,917,-361)),((544,-627,-890),(-476,619,847)),((423,-701,434),(-355,545,-477)),((459,-707,401),(-391,539,-444))]

    describe "find position" $ do
        it "tell the translation and rotation from one systems of coords to another" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            findPosition (sample!!0) (sample!!1)  `shouldBe` Just ((68,-1246,-43),[[-1,0,0],[0,1,0],[0,0,-1]]) 
            (translate (68,-1246,-43) (rotate [[-1,0,0],[0,1,0],[0,0,-1]] (-336,658,858))) `shouldBe` (404,-588,-901) 
        it "is nothing when one system has no common point to the other system" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            findPosition (sample!!0) (sample!!2) `shouldBe` Nothing 

    describe "acquire" $ do
        it "converts all the points of a scanner to a scanner within range" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            let cs = acquire (sample!!0) (sample!!1)
            length cs `shouldBe` length (sample!!0) + length (sample!!1) - length (intersection (sample!!0) (sample!!1))

    describe "acquire range" $ do
        it "converts all the points of scanners within range to origin scanner" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            let cds = acquireRange sample 1
            let ls x = length (sample !! x)
            length cds `shouldBe` ls 0 + ls 1 + ls 3 + ls 4 - length (intersection (sample!!0) (sample!!1)) - length (intersection (sample!!1) (sample!!3)) - length (intersection (sample!!1) (sample!!4))

    describe "acquire all ranges" $ do
        it "converts all points of all scanners to scanners within range" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            let cdss = acquireAllRanges sample 
            map length cdss `shouldBe` [38,65,40,38,53] 
            let sample' = map scan cdss
            let cdss' = acquireAllRanges sample'
            map length cdss'  `shouldBe` [79,79,79,79,79]
            let sample'' = map scan cdss'
            let cdss'' = acquireAllRanges sample''
            map length cdss''  `shouldBe` [79,79,79,79,79]
    describe "acquireAll" $ do
        it "converts all points of all scanners until all have the same size" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            map length (acquireAll sample) `shouldBe` [79,79,79,79,79] 
        it "should solve the puzzle part 1" $ do
            puzzle <- (readScanners . lines) <$> readFile "test/Puzzle19Data.txt"
            let cdss = acquireAllRanges puzzle
            map length cdss `shouldBe` 
                [53,68,39,80,53,68,54,82,54,39,40,53,39,52,40,51,53,0,40,68,53,54,95,52,68,40,52,68,39,39,81,54,53,66 ]
            let puzzle' = map scan cdss
            let cdss' = acquireAllRanges puzzle'
            map length cdss'  `shouldBe` [108,168,105,131,166,300,254,303,218,141,122,223,166,117,160,109,160,0,131,266,122,262,257,200,164,180,117,271,141,95,289,218,160,266]
            let puzzle'' = map scan cdss'
            let cdss'' = acquireAllRanges puzzle''
            map length cdss''  `shouldBe` [414,414,414,414,414,414,414,414,414,414,414,414,414,414,414,401,414,0,414,414,414,414,414,414,414,414,414,414,414,373,414,414,414,414]



