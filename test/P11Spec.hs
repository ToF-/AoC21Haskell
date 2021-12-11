module P11Spec
    where

import Test.Hspec
import P11

spec :: SpecWith ()
spec = do
    describe "step" $ do
        it "increase energy levels" $ do
            let grid=["11111"
                     ,"11111"
                     ,"11111"
                     ,"11111"
                     ,"11111"]
            step grid `shouldBe` ["22222"
                                 ,"22222"
                                 ,"22222"
                                 ,"22222"
                                 ,"22222"]
        it "cause octopuses with energy=9 to flash" $ do
            let grid=["11111"
                     ,"11111"
                     ,"11911"
                     ,"11111"
                     ,"11111"]
            step grid `shouldBe` ["11111"
                                 ,"12221"
                                 ,"12021"
                                 ,"12221"
                                 ,"11111"]


