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
                [Point {x = 0, y = 0, z = 0, is000 = True, distances = []}
                ,Point {x = 2, y = 3, z = 1, is000 = False, distances = [0,101,126]}
                ,Point {x = 8, y = -5, z = 0, is000 = False, distances = [101,0,165]}
                ,Point {x = 4, y = 2, z = -10, is000 = False, distances = [126,165,0]}]

    describe "readScanners" $ do
        it "read scanners from a file" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            head (tail (head sample)) `shouldBe` 
                Point {x = 404, y = -588, z = -901, is000 = False, distances = [0,1734501,5605830,19429,1136955,2401186,2281254,1292485,5937465,1178580,1004246,3827748,2277243,3214416,21242,1165776,4390731,5428385,3385779,1795355,1154534,1146209,3808714,3759893,1712390]}
            last (last sample) `shouldBe` 
                Point {x = 30, y = -46, z = -14, is000 = False, distances = [1224629,991242,800379,1395301,1445358,833333,1211276,1177106,577669,670424,37469,713284,1156030,1015017,1125425,1184870,1004955,1294605,763982,1313990,1311389,1277230,1091606,1374678,943704,0]}

    describe "rotations" $ do
        it "tell all rotations for a coord" $ do
            let (x,y,z) = (10,20,30)
            rotations (x,y,z) `shouldBe`
                [(x,y,z),(y,-x,z),(-x,-y,z),(-y,x,z),(-z,y,x),(-z,-x,y),(-z,-y,-x),(-z,x,-y),(-x,y,-z),(-y,-x,-z),(x,-y,-z),(y,x,-z),(z,y,-x),(z,-x,-y),(z,-y,x),(z,x,y),(x,z,-y),(y,z,x),(-x,z,y),(-y,z,-x),(-x,-z,-y),(-y,-z,x),(x,-z,y),(y,-z,-x)]

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

    describe "findPositions" $ do
        it "tell the translation and rotation from each scanner to every scanner" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            findPositions sample `shouldBe` 
                [((0,1),((68,-1246,-43),[[-1,0,0],[0,1,0],[0,0,-1]]))
                ,((1,0),((68,1246,-43),[[-1,0,0],[0,1,0],[0,0,-1]]))
                ,((1,3),((160,-1134,-23),[[1,0,0],[0,1,0],[0,0,1]]))
                ,((1,4),((88,113,-1104),[[0,0,-1],[1,0,0],[0,-1,0]]))
                ,((2,4),((1125,-168,72),[[0,1,0],[1,0,0],[0,0,-1]]))
                ,((3,1),((-160,1134,23),[[1,0,0],[0,1,0],[0,0,1]]))
                ,((4,1),((-1104,-88,113),[[0,1,0],[0,0,-1],[-1,0,0]]))
                ,((4,2),((168,-1125,72),[[0,1,0],[1,0,0],[0,0,-1]]))]

    describe "acquire" $ do
        it "converts all the points of a scanner to a scanner within range" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            let cs = acquire (sample!!0) (sample!!1)
            length cs `shouldBe` length (sample!!0) + length (sample!!1) - length (intersection (sample!!0) (sample!!1))

    describe "merge" $ do
        it "find positions and let the first scanner acquire the second" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            map size sample `shouldBe` [25,25,26,25,26]
            let sample' = merge sample
            map size sample' `shouldBe` [38,26,25,26]
            findPositions sample' `shouldBe`
                [((0,2),((-92,-2380,-20),[[-1,0,0],[0,1,0],[0,0,-1]]))
                ,((0,3),((-20,-1133,1061),[[0,0,1],[-1,0,0],[0,-1,0]]))
                ,((1,3),((1125,-168,72),[[0,1,0],[1,0,0],[0,0,-1]]))
                ,((2,0),((-92,2380,-20),[[-1,0,0],[0,1,0],[0,0,-1]]))
                ,((3,0),((-1061,-20,-1133),[[0,-1,0],[0,0,-1],[1,0,0]]))
                ,((3,1),((168,-1125,72),[[0,1,0],[1,0,0],[0,0,-1]]))]
            let sample'' = merge sample'
            map size sample'' `shouldBe` [51,26,26]
            findPositions sample'' `shouldBe`
                [((0,2),((-20,-1133,1061),[[0,0,1],[-1,0,0],[0,-1,0]]))
                ,((1,2),((1125,-168,72),[[0,1,0],[1,0,0],[0,0,-1]]))
                ,((2,0),((-1061,-20,-1133),[[0,-1,0],[0,0,-1],[1,0,0]]))
                ,((2,1),((168,-1125,72),[[0,1,0],[1,0,0],[0,0,-1]]))]
            let sample''' = merge sample''
            map size sample''' `shouldBe` [65,26]
            findPositions sample''' `shouldBe`
                [((0,1),((1105,-1205,1229),[[-1,0,0],[0,0,1],[0,1,0]]))
                ,((1,0),((1105,-1229,1205),[[-1,0,0],[0,0,1],[0,1,0]]))]
            let sample'''' = merge sample'''
            map size sample'''' `shouldBe` [79]
    describe "mergeAll" $ do
        it "merge scanner until there is only one scanner" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            let result = mergeAll sample
            size result `shouldBe` 79 

        it "should solve the first part of the puzzle" $ do
             puzzle <- (readScanners . lines) <$> readFile "test/Puzzle19Data.txt"
             print (map coord (puzzle !! 17))
             result <- mergeIO puzzle
             size result `shouldBe` 425 
    describe "manhattan distance" $ do
        it "tell the manhattan distance between two points" $ do
            manhattan (-92,-2380,100) `shouldBe` 2572
        it "should solve the sample" $ do
            sample <- (readScanners . lines) <$> readFile "test/Sample19Data.txt"
            let result = mergeAll sample
            size result `shouldBe` 79 
            origins result `shouldBe` [(-92,-2380,-20),(-20,-1133,1061),(0,0,0),(68,-1246,-43),(1105,-1205,1229)]
            largestManhattan result `shouldBe` 3621
        it "should solve the puzzle" $ do
            puzzle <- (readScanners . lines) <$> readFile "test/Puzzle19Data.txt"
            let result = mergeAll puzzle
            largestManhattan result `shouldBe` 13354
            



