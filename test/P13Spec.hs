module P13Spec
    where

import Test.Hspec
import P13
import qualified Data.Set as S

sample = ["6,10"
         ,"0,14"
         ,"9,10"
         ,"0,3"
         ,"10,4"
         ,"4,11"
         ,"6,0"
         ,"6,12"
         ,"4,1"
         ,"0,13"
         ,"10,12"
         ,"3,4"
         ,"3,0"
         ,"8,4"
         ,"1,10"
         ,"2,14"
         ,"8,10"
         ,"9,0"
         ,""
         ,"fold along y=7"
         ,"fold along x=5"]


p= page sample

spec :: SpecWith ()
spec = do
    describe "page" $ do
        it "makes a set of the coordinates" $ do
            S.toList p `shouldBe` 
                [(0,3),(0,13),(0,14),(1,10),(2,14),(3,0),(3,4),(4,1),(4,11),(6,0),(6,10),(6,12),(8,4),(8,10),(9,0),(9,10),(10,4),(10,12)]
    describe "foldY" $ do
        it "folds the page along a horizontal line, merging the dots" $ do
            let r = foldY 7 p
            S.toList r `shouldBe` 
                [(0,0),(0,1),(0,3),(1,4),(2,0),(3,0),(3,4),(4,1),(4,3),(6,0),(6,2),(6,4),(8,4),(9,0),(9,4),(10,2),(10,4)]

    describe "foldX" $ do
        it "folds the page along a vertical line, mergine the dots" $ do
            let r = foldX 5 (foldY 7 p)
            S.toList r `shouldBe` 
                [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,4),(2,0),(2,4),(3,0),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)]

    describe "execute" $ do
        it "execute instruction given on the page" $ do
            S.toList (execute sample) `shouldBe`   
                [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,4),(2,0),(2,4),(3,0),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)]


    describe "executeFirst" $ do
        it "execute the first instruction given on the page" $ do
            S.toList (executeFirst sample) `shouldBe`   
                [(0,0),(0,1),(0,3),(1,4),(2,0),(3,0),(3,4),(4,1),(4,3),(6,0),(6,2),(6,4),(8,4),(9,0),(9,4),(10,2),(10,4)]

    describe "solutionA" $ do
        it "should solve the sample" $ do
            solutionA sample `shouldBe` 17 
        it "souhld solve the puzzle" $ do
            puzzle <- lines <$> readFile "test/Puzzle13Data.txt"
            solutionA puzzle `shouldBe`  684


    describe "display" $ do
        it "display a page" $ do
            display (execute sample) `shouldBe` 
                ["#####"
                ,"#...#"
                ,"#...#"
                ,"#...#"
                ,"#####"]

        it "should solve the puzzle" $ do
            puzzle <- lines <$> readFile "test/Puzzle13Data.txt"
            display (execute puzzle) `shouldBe`   -- JRZBLGKH
                ["..##.###..####.###..#.....##..#..#.#..#"
                ,"...#.#..#....#.#..#.#....#..#.#.#..#..#"
                ,"...#.#..#...#..###..#....#....##...####"
                ,"...#.###...#...#..#.#....#.##.#.#..#..#"
                ,"#..#.#.#..#....#..#.#....#..#.#.#..#..#"
                ,".##..#..#.####.###..####..###.#..#.#..#"]
