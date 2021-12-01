module P01Spec
    where

import Test.Hspec
import P01
import Puzzle01Data

spec :: SpecWith ()
spec = do
    describe "increases" $ do
        it "passes a small sample" $ do
            increases [ 199, 200, 208, 210, 200, 207, 240, 269, 260, 263 ]
                 `shouldBe` 7
        it "passes the puzzle input" $ do
            increases puzzle01 `shouldBe` 1791

    describe "3 measurement increases" $ do
        it "passes a small sample" $ do
            windowIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] 
                 `shouldBe` 5

        it "passes the puzzle input" $ do
            windowIncreases puzzle01 `shouldBe` 1822

