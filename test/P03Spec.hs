module P03Spec
    where

import Test.Hspec
import P03
import Puzzle03Data
import Data.List


sample = map binary ["00100"
                    ,"11110"
                    ,"10110"
                    ,"10111"
                    ,"10101"
                    ,"01111"
                    ,"00111"
                    ,"11100"
                    ,"10000"
                    ,"11001"
                    ,"00010"
                    ,"01010"]
spec :: SpecWith ()
spec = do
    describe "most common" $ do
        it "should tell the most common bit" $ do
            mostCommon [1,1,1,0] `shouldBe` 1
            mostCommon [0,0,1,0] `shouldBe` 0
            mostCommon [1,0,0,0,1,0,1]  `shouldBe` 0
        it "should select 1 in case of equality" $ do
            mostCommon [1,1,0,0] `shouldBe` 1
    describe "most common bit pattern" $ do
        it "should produce a bit pattern" $ do
            let pattern = mostCommonBitsPattern sample 
            pattern `shouldBe` [1,0,1,1,0]
            binaryToInt pattern `shouldBe` 22

    describe "least common bit pattern" $ do
        it "should produce a bit pattern" $ do
            leastCommonBitsPattern sample `shouldBe` [0,1,0,0,1]

    describe "solution" $ do
        it "to the sample imput" $ do
            let g = gammaRate sample
            let e = epsilonRate sample
            g * e `shouldBe` 198
        it "to the puzzle imput" $ do
            let patterns = map binary puzzle
            let g = gammaRate patterns
            let e = epsilonRate patterns
            g * e `shouldBe` 2967914

    describe "reduce by most common bit" $ do
        it "should keep only pattern with most common bit in a position" $ do
            let result = reduceByMostCommonBit 0 sample
            result `shouldBe` [[1,1,1,1,0]
                              ,[1,0,1,1,0]
                              ,[1,0,1,1,1]
                              ,[1,0,1,0,1]
                              ,[1,1,1,0,0]
                              ,[1,0,0,0,0]
                              ,[1,1,0,0,1]]
            let result' = reduceByMostCommonBit 1 result
            result' `shouldBe` [[1,0,1,1,0]
                               ,[1,0,1,1,1]
                               ,[1,0,1,0,1]
                               ,[1,0,0,0,0]]
            let result'' = reduceByMostCommonBit 2 result'
            result'' `shouldBe` [[1,0,1,1,0]
                                ,[1,0,1,1,1]
                                ,[1,0,1,0,1]]
            reduceByMostCommonBits sample `shouldBe` [1,0,1,1,1]
    describe "reduce by least common bit" $ do
        it "should keep only patterns with least common bit in a position" $ do
            reduceByLeastCommonBits sample `shouldBe` [0,1,0,1,0]

    describe "oxygen generator rating" $ do
        it "should convert most common bit reduction" $ do
            oxygenGeneratorRating sample `shouldBe` 23

    describe "C02 scrubber rating" $ do
        it "should convert least common bit reduction" $ do
            co2ScrubberRating sample `shouldBe` 10

    describe "this should solve" $ do
        it "the sample" $ do
            (oxygenGeneratorRating sample) * (co2ScrubberRating sample) `shouldBe` 230
        it "the puzzle" $ do
            let p = map binary puzzle
            (oxygenGeneratorRating p) * (co2ScrubberRating p) `shouldBe` 7041258
            
