module P11Spec
    where

import Test.Hspec
import P11

puzzle = ["6744638455"
         ,"3135745418"
         ,"4754123271"
         ,"4224257161"
         ,"8167186546"
         ,"2268577674"
         ,"7177768175"
         ,"2662255275"
         ,"4655343376"
         ,"7852526168"]
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
        it "cause octopuses with energy=9 to flash and their energy to ripple" $ do
            let grid  =  ["11111"
                         ,"19991"
                         ,"19191"
                         ,"19991"
                         ,"11111"]
            let result = ["34543"
                         ,"40004"
                         ,"50005"
                         ,"40004"
                         ,"34543"]
            step grid `shouldBe` result
            step (step grid) `shouldBe` ["45654"
                                        ,"51115"
                                        ,"61116"
                                        ,"51115"
                                        ,"45654"]

    describe "countFlashes" $ do
        it "count the flashed octopuses given a number of steps" $ do
            countFlashes 1 sample `shouldBe` 0
            countFlashes 2 sample `shouldBe` 35
            countFlashes 10 sample `shouldBe` 204
            countFlashes 100 sample `shouldBe` 1656

        it "should pass the puzzle" $ do
            countFlashes 100 puzzle `shouldBe` 1608

    describe "firstStepWithAllFlashes" $ do
        it "tells the first step at which all the octopuses flash" $ do
            firstStepWithAllFlash sample `shouldBe` Just 195

        it "should pass the puzzle" $ do
            firstStepWithAllFlash puzzle `shouldBe` Just 214
