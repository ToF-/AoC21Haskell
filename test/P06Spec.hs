module P06Spec
    where

import Test.Hspec
import P06

sample = [3,4,3,1,2]
puzzle = [3,5,3,5,1,3,1,1,5,5,1,1,1,2,2,2,3,1,1,5,1,1,5,5,3,2,2,5,4,4,1,5,1,4,4,5,2,4,1,1,5,3,1,1,4,1,1,1,1,4,1,1,1,1,2,1,1,4,1,1,1,2,3,5,5,1,1,3,1,4,1,3,4,5,1,4,5,1,1,4,1,3,1,5,1,2,1,1,2,1,4,1,1,1,4,4,3,1,1,1,1,1,4,1,4,5,2,1,4,5,4,1,1,1,2,2,1,4,4,1,1,4,1,1,1,2,3,4,2,4,1,1,5,4,2,1,5,1,1,5,1,2,1,1,1,5,5,2,1,4,3,1,2,2,4,1,2,1,1,5,1,3,2,4,3,1,4,3,1,2,1,1,1,1,1,4,3,3,1,3,1,1,5,1,1,1,1,3,3,1,3,5,1,5,5,2,1,2,1,4,2,3,4,1,4,2,4,2,5,3,4,3,5,1,2,1,1,4,1,3,5,1,4,1,2,4,3,1,5,1,1,2,2,4,2,3,1,1,1,5,2,1,4,1,1,1,4,1,3,3,2,4,1,4,2,5,1,5,2,1,4,1,3,1,2,5,5,4,1,2,3,3,2,2,1,3,3,1,4,4,1,1,4,1,1,5,1,2,4,2,1,4,1,1,4,3,5,1,2,1]
spec :: SpecWith ()
spec = do
    describe "generation" $ do
        it "decrease internal timers" $ do
            generation sample `shouldBe` [2,3,2,0,1]
        it "change 0 in 6 and add an 8 with each 0" $ do
            generation (generation sample) `shouldBe` [1,2,1,6,0,8]

    describe "nthGeneration" $ do
        it "gives the nth generation from initial" $ do
           sample `nthGeneration` 1 `shouldBe` [2,3,2,0,1]
           sample `nthGeneration` 18 `shouldBe` [6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8]

        it "should pass the sample" $ do
            length (sample `nthGeneration` 80) `shouldBe` 5934

        it "should pass the puzzle" $ do
            length (puzzle `nthGeneration` 80) `shouldBe` 365862

    describe "tally" $ do
        it "count each timer" $ do
            tally sample  `shouldBe` [0,1,1,2,1,0,0,0,0]


    describe "evolve" $ do
        it "states the tally after a generation" $ do
            let t = tally sample 
            evolve t `shouldBe` [1,1,2,1,0,0,0,0,0]

        it "makes 0s become 8s and add to 6s" $ do
            let t = tally sample
            evolve (evolve t) `shouldBe` [1,2,1,0,0,0,1,0,1]

    describe "nthEvolution" $ do
        it "give the nth evolution from initial tall" $ do
            (tally sample) `nthEvolution` 1 `shouldBe` [1,1,2,1,0,0,0,0,0] 
            (tally sample) `nthEvolution` 18 `shouldBe` [3,5,3,2,2,1,5,1,4] 

        it "should pass the sample" $ do
            sum ((tally sample) `nthEvolution` 80)  `shouldBe` 5934 

        it "should pass the puzzle" $ do
            sum ((tally puzzle) `nthEvolution` 80) `shouldBe` 365862

        it "should pass the puzzle for 256 generations" $ do
            sum ((tally puzzle) `nthEvolution` 256) `shouldBe`  1653250886439
