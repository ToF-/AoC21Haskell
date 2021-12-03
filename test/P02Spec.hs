module P02Spec
    where

import Test.Hspec
import P02
import Puzzle02Data

spec :: SpecWith ()
spec = do
    describe "final position" $ do
        it "for sample" $ do
            let course = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]
            finalPosition course `shouldBe` (15,10)
        it "for puzzle" $ do
            finalPosition puzzleCourse `shouldBe` (1895,894)
            let (h,d) = finalPosition puzzleCourse
            h * d  `shouldBe` 1694130

    describe "final position with aim" $ do
        it "for sample" $ do
            let course = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]
            finalPositionWithAim course `shouldBe` (15,10,60)
        it "for puzzle" $ do
            finalPositionWithAim puzzleCourse `shouldBe` (1895,894,896491)
            let (h,a,d) = finalPositionWithAim puzzleCourse
            h * d  `shouldBe` 1698850445

