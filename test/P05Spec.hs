module P05Spec
    where

import Test.Hspec
import P05
import Puzzle05Data

sampleInput = [((0,9),(5,9))  -- i.f....eb.
              ,((8,0),(0,8))  -- .idf...e..
              ,((9,4),(3,4))  -- ..i.f.bej.
              ,((2,2),(2,1))  -- ...i.f.j..
              ,((7,0),(7,4))  -- .hhhicjecc
              ,((6,4),(2,0))  -- ...b.j....
              ,((0,9),(2,9))  -- ..b...i...
              ,((3,4),(1,4))  -- .b.....i..
              ,((0,0),(8,8))  -- b.......i.
              ,((5,5),(8,2))]  -- gggaaa....

spec :: SpecWith ()
spec = do
    describe "allPointsHV" $ do
        it "should sanity check point mapping" $ do
            allPointsHV sampleInput `shouldBe`
                [((0,9),2),((1,4),1),((1,9),2),((2,1),1),((2,2),1),((2,4),1),((2,9),2),((3,4),2),((3,9),1),((4,4),1),((4,9),1),((5,4),1),((5,9),1),((6,4),1),((7,0),1),((7,1),1),((7,2),1),((7,3),1),((7,4),2),((8,4),1),((9,4),1)]
-- .......1..
-- ..1....1..
-- ..1....1..
-- .......1..
-- .112111211
-- ..........
-- ..........
-- ..........
-- ..........
-- 222111....

    describe "overlapsHV" $ do
        it "should tell which points where at least two horizontal or vertical lines overlap" $ do
            overlapsHV sampleInput `shouldBe` [((0,9),2),((1,9),2),((2,9),2),((3,4),2),((7,4),2)]
        it "should pass the puzzle" $ do
            length (overlapsHV puzzleInput) `shouldBe` 8111

    describe "display" $ do
        it "should sanity check point mapping" $ do
            display (plane sampleInput) `shouldBe` 
                ["1.1....11."
                ,".111...2.."
                ,"..2.1.111."
                ,"...1.2.2.."
                ,".112313211"
                ,"...1.2...."
                ,"..1...1..."
                ,".1.....1.."
                ,"1.......1."
                ,"222111...."]

    describe "overlapsHVD" $ do
        it "should tell which points where at least two horizontal or vertical or diagonal lines overlap" $ do
            overlapsHVD sampleInput `shouldBe`
               [((0,9),2),((1,9),2),((2,2),2),((2,9),2),((3,4),2),((4,4),3),((5,3),2),((5,5),2),((6,4),3),((7,1),2),((7,3),2),((7,4),2)]
        it "should pass the puzzle" $ do
            length (overlapsHVD puzzleInput) `shouldBe` 22088
