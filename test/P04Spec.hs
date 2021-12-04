module P04Spec
    where

import Test.Hspec
import P04

sampleInput = ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
              ,""
              ,"22 13 17 11  0"
              ," 8  2 23  4 24"
              ,"21  9 14 16  7"
              ," 6 10  3 18  5"
              ," 1 12 20 15 19"
              ,""
              ," 3 15  0  2 22"
              ," 9 18 13 17  5"
              ,"19  8  7 25 23"
              ,"20 11 10 24  4"
              ,"14 21 16 12  6"
              ,""
              ,"14 21 17 24  4"
              ,"10 16 15  9 19"
              ,"18  8 23 26 20"
              ,"22 11 13  6  5"
              ," 2  0 12  3  7"]
bingo = readBingo sampleInput

spec :: SpecWith ()
spec = do
    describe "read bingo" $ do
        it "should read the input data from a list of strings" $ do
            numbers bingo `shouldBe` [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
            lastCalled bingo `shouldBe` Nothing
            (grids bingo) !! 0 `shouldBe`
                        [[(22,False),(13,False),(17,False),(11,False), (0,False)]
                        ,[ (8,False), (2,False),(23,False), (4,False),(24,False)]
                        ,[(21,False), (9,False),(14,False),(16,False), (7,False)]
                        ,[ (6,False),(10,False), (3,False),(18,False), (5,False)]
                        ,[ (1,False),(12,False),(20,False),(15,False),(19,False)]]
            (grids bingo) !! 2 `shouldBe`
                        [[(14,False),(21,False),(17,False),(24,False), (4,False)]
                        ,[(10,False),(16,False),(15,False), (9,False),(19,False)]
                        ,[(18,False), (8,False),(23,False),(26,False),(20,False)]
                        ,[(22,False),(11,False),(13,False), (6,False), (5,False)]
                        ,[ (2,False), (0,False),(12,False), (3,False), (7,False)]]

    describe "draw" $ do
        it "should mark the grids with the number drawn" $ do
            let bingo' = draw bingo
            lastCalled bingo'  `shouldBe` Just 7
            numbers bingo' `shouldBe` tail (numbers bingo)
            (grids bingo') !! 0 `shouldBe`
                        [[(22,False),(13,False),(17,False),(11,False), (0,False)]
                        ,[ (8,False), (2,False),(23,False), (4,False),(24,False)]
                        ,[(21,False), (9,False),(14,False),(16,False), (7,True)]
                        ,[ (6,False),(10,False), (3,False),(18,False), (5,False)]
                        ,[ (1,False),(12,False),(20,False),(15,False),(19,False)]]
            (grids bingo') !! 2 `shouldBe`
                        [[(14,False),(21,False),(17,False),(24,False), (4,False)]
                        ,[(10,False),(16,False),(15,False), (9,False),(19,False)]
                        ,[(18,False), (8,False),(23,False),(26,False),(20,False)]
                        ,[(22,False),(11,False),(13,False), (6,False), (5,False)]
                        ,[ (2,False), (0,False),(12,False), (3,False), (7,True)]]

    describe "winner" $ do
        it "should be the grid with a completed row or column" $ do
            winner bingo `shouldBe` Nothing
            let bingos = iterate draw bingo
            let bingo' = last (take 13 bingos)
            lastCalled bingo' `shouldBe` Just 24
            head (numbers bingo') `shouldBe` 10
            winner bingo' `shouldBe`
               Just [[(14,True ),(21,True ),(17,True ),(24,True ), (4,True )]
                    ,[(10,False),(16,False),(15,False), (9,True ),(19,False)]
                    ,[(18,False), (8,False),(23,True ),(26,False),(20,False)]
                    ,[(22,False),(11,True ),(13,False), (6,False), (5,True )]
                    ,[ (2,True ), (0,True ),(12,False), (3,False), (7,True)]]

            let newBingo = bingo { numbers = [22,8,21,6,1,7,4,9,5] }
            let newBingos = iterate draw newBingo
            let newBingo'' = last (take 6 newBingos)
            lastCalled newBingo'' `shouldBe` Just 1
            winner newBingo'' `shouldBe`
                Just [[(22,True ),(13,False),(17,False),(11,False), (0,False)]
                     ,[ (8,True ), (2,False),(23,False), (4,False),(24,False)]
                     ,[(21,True ), (9,False),(14,False),(16,False), (7,False)]
                     ,[ (6,True ),(10,False), (3,False),(18,False), (5,False)]
                     ,[ (1,True ),(12,False),(20,False),(15,False),(19,False)]]

    describe "solution A" $ do
        describe "should multiply the last number called by the sum of non marked numbers of the winner" $ do
            it "should pass the sample" $ do
                let bingos = iterate draw bingo
                let bingo' = last (take 13 bingos)
                lastCalled bingo' `shouldBe` Just 24
                head (numbers bingo') `shouldBe` 10
                winner bingo' `shouldBe`
                   Just [[(14,True ),(21,True ),(17,True ),(24,True ), (4,True )]
                        ,[(10,False),(16,False),(15,False), (9,True ),(19,False)]
                        ,[(18,False), (8,False),(23,True ),(26,False),(20,False)]
                        ,[(22,False),(11,True ),(13,False), (6,False), (5,True )]
                        ,[ (2,True ), (0,True ),(12,False), (3,False), (7,True)]]
                solutionA bingo' `shouldBe` Just 4512
            it "should pass the sample" $ do
                input <- readFile "Puzzle04Data.txt"
                let bingo = readBingo (lines input)
                solutionA bingo `shouldBe` Just 64084

