module P19Spec
    where

import Test.Hspec
import P19
import Sample19Data 
import Data.List

spec :: SpecWith ()
spec = do
    describe "distance" $ do
        it "tell the distance between 2 points in space" $ do
            let p1 = point (2, 3, 1)
            let p2 = point (8,-5, 0)
            abs (distance p1 p2 - 10.05) < 0.01 `shouldBe` True 

    describe "signature" $ do
        it "tell the distance from a point to all other points in the scanner range" $ do
            let p1 = point (2,3,1)
            let pts = map point [(8,-5,0),(4,2,-10),(-3,7,18)]
            signature p1 pts `shouldBe` [100499,112250,181659]

    describe "report" $ do
        it "tell all the points and their signatures from a scanner" $ do
            let scanner0 = map point [(2,3,1),(8,-5,0),(4,2,-10),(-3,7,18)]
            let r0 = report scanner0
            r0 `shouldBe`
                [(Point {x = 2, y = 3, z = 1},[100499,112250,181659])
                ,(Point {x = 8, y = -5, z = 0},[100499,128452,242693])
                ,(Point {x = 4, y = 2, z = -10},[112250,128452,292916])
                ,(Point {x = -3, y = 7, z = 18},[181659,242693,292916])]

            let scanner1 = map point [(42,3,17),(22,-2,11),(28,0,10),(24,-3,0),(17,2,28),(19,-28,7)]
            let r1 = report scanner1
            r1 `shouldBe`
                [(Point {x = 42, y = 3, z = 17},[159374,214709,254755,273313,398748])
                ,(Point {x = 22, y = -2, z = 11},[64031,112250,181659,214709,264764])
                ,(Point {x = 28, y = 0, z = 10},[64031,111803,159374,211896,295635])
                ,(Point {x = 24, y = -3, z = 0},[111803,112250,254755,264386,292916])
                ,(Point {x = 17, y = 2, z = 28},[181659,211896,273313,292916,366742])
                ,(Point {x = 19, y = -28, z = 7},[264386,264764,295635,366742,398748])]      
    describe "commonPoints" $ do
        it "tell the points from two reports with a certain number of distances they have in common in their signature" $ do
            let scanner0 = map point [(17,-3,5),(2,3,1),(8,-5,0),(4,2,-10),(-3,7,18)]
            let scanner1 = map point [(42,3,17),(22,-2,11),(28,0,10),(24,-3,0),(17,2,28),(19,-28,7)]
            commonPoints scanner0 scanner1 2 `shouldBe` 
                [(Point {x = 2, y = 3, z = 1},Point {x = 22, y = -2, z = 11})
                ,(Point {x = 4, y = 2, z = -10},Point {x = 24, y = -3, z = 0})
                ,(Point {x = -3, y = 7, z = 18},Point {x = 17, y = 2, z = 28})]
        it "should pass the sample" $ do
            let scanner0 = map point (sampleScanners !! 0)
            let scanner1 = map point (sampleScanners !! 1)
            (commonPoints scanner0 scanner1 11) `shouldBe`
                [(Point {x = 404, y = -588, z = -901},Point {x = -336, y = 658, z = 858})
                ,(Point {x = 528, y = -643, z = 409},Point {x = -460, y = 603, z = -452})
                ,(Point {x = 390, y = -675, z = -793},Point {x = -322, y = 571, z = 750})
                ,(Point {x = -537, y = -823, z = -458},Point {x = 605, y = 423, z = 415})
                ,(Point {x = -485, y = -357, z = 347},Point {x = 553, y = 889, z = -390})
                ,(Point {x = -345, y = -311, z = 381},Point {x = 413, y = 935, z = -424})
                ,(Point {x = -661, y = -816, z = -575},Point {x = 729, y = 430, z = 532})
                ,(Point {x = -618, y = -824, z = -621},Point {x = 686, y = 422, z = 578})
                ,(Point {x = -447, y = -329, z = 318},Point {x = 515, y = 917, z = -361})
                ,(Point {x = 544, y = -627, z = -890},Point {x = -476, y = 619, z = 847})
                ,(Point {x = 423, y = -701, z = 434},Point {x = -355, y = 545, z = -477})
                ,(Point {x = 459, y = -707, z = 401},Point {x = -391, y = 539, z = -444})]   
    describe "rotate" $ do
        it "rotate a point" $ do
            let p = point (34, 2, 18)
            (p `rotate` (rotations !! 0))`shouldBe` point ( 34, 2, 18)
    describe "rotateCommonPoints" $ do
        it "rotate all the second points of a list of common points" $ do
            let scanner0 = map point (sampleScanners !! 0)
            let scanner1 = map point (sampleScanners !! 1)
            (rotateCommonPoints (commonPoints scanner0 scanner1 11) (rotations!!4)) `shouldBe`
                [(Point {x = 404, y = -588, z = -901},Point {x = 658, y = 336, z = 858})
                ,(Point {x = 528, y = -643, z = 409},Point {x = 603, y = 460, z = -452})
                ,(Point {x = 390, y = -675, z = -793},Point {x = 571, y = 322, z = 750})
                ,(Point {x = -537, y = -823, z = -458},Point {x = 423, y = -605, z = 415})
                ,(Point {x = -485, y = -357, z = 347},Point {x = 889, y = -553, z = -390})
                ,(Point {x = -345, y = -311, z = 381},Point {x = 935, y = -413, z = -424})
                ,(Point {x = -661, y = -816, z = -575},Point {x = 430, y = -729, z = 532})
                ,(Point {x = -618, y = -824, z = -621},Point {x = 422, y = -686, z = 578})
                ,(Point {x = -447, y = -329, z = 318},Point {x = 917, y = -515, z = -361})
                ,(Point {x = 544, y = -627, z = -890},Point {x = 619, y = 476, z = 847})
                ,(Point {x = 423, y = -701, z = 434},Point {x = 545, y = 355, z = -477})
                ,(Point {x = 459, y = -707, z = 401},Point {x = 539, y = 391, z = -444})]
    describe "translation" $ do
        it "tell the difference between two points coordinates relatively" $ do
            translation (point (404,-588,-901)) (point (658,336,858)) `shouldBe` (404-658,-588-336,-901-858) 

    describe "isHomogen" $ do
        it "tell if a rotation yields the same translation for all common points" $ do
                let cps = [(Point {x = 2, y = 3, z = 1},Point {x = 22, y = -2, z = 11})
                          ,(Point {x = 4, y = 2, z = -10},Point {x = 24, y = -3, z = 0})
                          ,(Point {x = -3, y = 7, z = 18},Point {x = 17, y = 2, z = 28})]
                isHomogen (cps, rotations!!0) `shouldBe` True 
                isHomogen (rotateCommonPoints cps (rotations!!4), rotations!!4) `shouldBe` False 

    describe "allRotations" $ do
        it "is the list of all possible rotations" $ do
            let p = point (42, 17, -23)
            let ps = map (rotate p) rotations
            ps `shouldBe` [Point {x = 42, y = 17, z = -23}
                          ,Point {x = 42, y = -23, z = -17}
                          ,Point {x = 42, y = -17, z = 23}
                          ,Point {x = 42, y = 23, z = 17}
                          ,Point {x = 17, y = -42, z = -23}
                          ,Point {x = 17, y = -23, z = 42}
                          ,Point {x = 17, y = 42, z = 23}
                          ,Point {x = 17, y = 23, z = -42}
                          ,Point {x = 42, y = -17, z = -23}
                          ,Point {x = -42, y = 23, z = -17}
                          ,Point {x = -42, y = 17, z = 23}
                          ,Point {x = -42, y = -23, z = 17}
                          ,Point {x = -17, y = 42, z = -23}
                          ,Point {x = -17, y = 23, z = 42}
                          ,Point {x = -17, y = -42, z = 23}
                          ,Point {x = -17, y = -23, z = -42}
                          ,Point {x = -23, y = 17, z = -42}
                          ,Point {x = -23, y = 42, z = 17}
                          ,Point {x = -23, y = -17, z = 42}
                          ,Point {x = -23, y = -42, z = -17}
                          ,Point {x = 23, y = -17, z = -42}
                          ,Point {x = 23, y = -42, z = 17}
                          ,Point {x = 23, y = 17, z = 42}
                          ,Point {x = 23, y = 42, z = -17}]

    describe "findHomogen" $ do
        it "find the first rotation to create an homogen translation of common points" $ do
            let scanner0 = map point (sampleScanners !! 0)
            let scanner1 = map point (sampleScanners !! 1)
            let scanner4 = map point (sampleScanners !! 4)
            let cps0_1 = sort $ commonPoints scanner0 scanner1 11
            let firstCommon0_1 = cps0_1 !! 1
            let fstc0_1 = fst firstCommon0_1
            let sndc0_1 = snd firstCommon0_1
            coords fstc0_1 `shouldBe` (-618,-824,-621)
            coords sndc0_1 `shouldBe` (686,422,578)
            let Just (t1,r1) = findHomogen cps0_1
            t1 `shouldBe` (68,-1246,-43)
            r1 `shouldBe` [[-1,0,0],[0,1,0],[0,0,-1]]
            translationT (coords fstc0_1) (coords (rotate sndc0_1 r1)) `shouldBe` (68,-1246,-43)
            ((flip translate t1).(flip rotate r1)) sndc0_1 `shouldBe` fstc0_1 

            let cps1_4 = commonPoints scanner1 scanner4 11
            let firstCommon1_4 = cps1_4 !! 1
            let fstc1_4 = fst firstCommon1_4
            let sndc1_4 = snd firstCommon1_4
            coords fstc1_4 `shouldBe` (-340,-569,-846)
            coords sndc1_4 `shouldBe` (-258,-428,682)
            let Just (t4,r4) = findHomogen cps1_4
            t4 `shouldBe` (88,113,-1104)
            r4 `shouldBe` [[0,0,-1],[1,0,0],[0,-1,0]]


            let cps1tr_4 = commonPoints (map ((flip translate t1) . (flip rotate r1)) scanner1) scanner4 11
            let firstCommon1tr_4 = cps1tr_4 !! 1
            let fstc1tr_4 = fst firstCommon1tr_4
            let sndc1tr_4 = snd firstCommon1tr_4
            coords fstc1tr_4 `shouldBe` (408,-1815,803)
            coords sndc1tr_4 `shouldBe` (-258,-428,682)
            let Just (t14,r14) = findHomogen cps1tr_4
            t14 `shouldBe` (-20,-1133,1061)
            r14 `shouldBe` [[0,0,1],[-1,0,0],[0,-1,0]]

    describe "assemble" $ do
        it "add points of a scanner after translating and rotating them accordingly" $ do
            let scanner0 = map point (sampleScanners !! 0)
            let scanner1 = map point (sampleScanners !! 1)
            let result = assemble scanner0 scanner1
            length result `shouldBe` length scanner0 + length scanner1 - length (commonPoints scanner0 scanner1 11) 

    describe "merge program" $ do
        it "tell all the assemblies to make" $ do
            mergeProgram 5  `shouldBe` [[(0,1),(0,2),(0,3),(0,4)],[(1,2),(1,3),(1,4)],[(2,3),(2,4)],[(3,4)]]

    describe "merge" $ do
        it "merge assembled scanners according to merge program" $ do
            let result = mergeAll (map (map point) sampleScanners)
            length result `shouldBe` 79

    describe "readScanners" $ do
        it "convert lines into a list of scanners" $ do 
            let input = ["--- scanner 0 ---"
                        ,""
                        ,"404,-588,-901"
                        ,"528,-643,409"
                        ,"-838,591,734"
                        ,""
                        ,"--- scanner 1 ---"
                        ,""
                        ,"553,345,-567"
                        ,"474,580,667"
                        ,"-447,-329,318"]
            let scs = readScanners input
            length scs  `shouldBe` 2 
            head (head scs) `shouldBe` point (404,-588,-901)  
            last (last scs) `shouldBe` point (-447,-329,318)

    describe "all of this" $ do
        it "should pass the puzzle part A" $ do
            content <- lines <$> readFile "test/Puzzle19Data.txt"
            let puzzle = readScanners content
            length puzzle `shouldBe` 34 
            head (head puzzle) `shouldBe` point (562,-830,765) 
            last (last puzzle) `shouldBe` point (682,-518,447) 
            let result = mergeAll puzzle
            length result `shouldBe` 0 
        
