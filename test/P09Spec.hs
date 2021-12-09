module P09Spec
    where

import Test.Hspec
import P09

sample = ["2199943210" ,"3987894921" ,"9856789892" ,"8767896789" ,"9899965678"]


spec :: SpecWith ()
spec = do
    describe "riskLevels" $ do
        it "tells the risk level of those points which are lower than their adjacent points" $ do
            sum (riskLevels sample) `shouldBe` 15

        it "should pass the first part of the puzzle" $ do
            input <- lines <$> readFile "test/Puzzle09Data.txt"
            sum (riskLevels input) `shouldBe` 439

    describe "basin" $ do
        it "tells the coords of a basin flowing towards a coord" $ do
            let hm = heightMap sample
            lowestCoords hm `shouldBe` [(0,1),(0,9),(2,2),(4,6)]
            basin hm (0,1) `shouldBe` [(0,1),(0,0),(1,0)]
            basin hm (0,9) `shouldBe` [(0,9),(0,8),(0,7),(0,6),(0,5),(1,6),(1,8),(1,9),(2,9)]
    describe "three largest basin sizes" $ do
        it "solve the sample" $ do
            let hm = heightMap sample
            product (threeLargestBasinSizes hm) `shouldBe` 1134
        it "solve the puzzle" $ do
            input <- lines <$> readFile "test/Puzzle09Data.txt"
            let hm = heightMap input
            product (threeLargestBasinSizes hm) `shouldBe` 900900

