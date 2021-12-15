module P14Spec
    where

import Test.Hspec
import P14
import P14Puzzle
import qualified Data.Map as M
import qualified Data.List as L


sample = [("CH",'B')
         ,("HH",'N')
         ,("CB",'H')
         ,("NH",'C')
         ,("HB",'C')
         ,("HC",'B')
         ,("HN",'C')
         ,("NN",'C')
         ,("BH",'H')
         ,("NC",'B')
         ,("NB",'B')
         ,("BN",'B')
         ,("BB",'N')
         ,("BC",'B')
         ,("CC",'N')
         ,("CN",'C')]

gs = L.sort . L.map (\g -> (head g, length g)) . L.group . L.sort

spec :: SpecWith ()
spec = do
    describe "step" $ do
        it "insert elements according tp pair insertion rules" $ do
            step sample "NNCB" `shouldBe` "NCNBCHB"


    describe "afterStep" $ do
        it "execute n steps of insertion process" $ do
            afterStep "NNCB" sample 1 `shouldBe` step sample "NNCB"
            afterStep "NNCB" sample 4 `shouldBe` "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"

    describe "leastAndMostCommon" $ do
        it "tells the quantity of the least and most common elements in a template" $ do
            leastAndMostCommon (afterStep "NNCB" sample 10) `shouldBe` ((161,'H'),(1749,'B'))

        it "should solve the puzzle" $ do
            let ((l,_),(m,_)) = leastAndMostCommon (afterStep initial puzzle 10)
            (l,m) `shouldBe` (706,3296)
            m-l `shouldBe`  2590

    describe "initialCounter" $ do
        it "make a counter of the initial template" $ do
            let c = initialCounter "NNCB" 
            (L.sort . M.toList) c `shouldBe` [("B",0),("C",-1),("CB",1),("N",-1),("NC",1),("NN",1)]
            (L.sort . M.toList) (separate c) `shouldBe`[('B',1),('C',1),('N',2)]

    describe "stepCounter" $ do
        it "updates a counter according to the rules" $ do
            let c = initialCounter "NNCB"
                r = stepCounter sample c
                s = stepCounter sample r
            (L.sort . M.toList) r `shouldBe` [("B",-1),("BC",1),("C",-2),("CH",1),("CN",1),("H",-1),("HB",1),("N",-1),("NB",1),("NC",1)]
                
            (L.sort . M.toList) (separate r)`shouldBe` [('B',2),('C',2),('H',1),('N',2)]  
            (L.sort . M.toList) (separate r)`shouldBe` gs "NCNBCHB"
            (L.sort . M.toList) (separate s)`shouldBe` gs "NBCCNBBBCBHCB"

    describe "afterStepCounter" $ do
        it "execute n steps of insertion process" $ do
            let r = afterStepCounter "NNCB" sample 1 
            let s = afterStepCounter "NNCB" sample 4 
            (L.sort . M.toList) (separate r) `shouldBe` gs "NCNBCHB"
            (L.sort . M.toList) (separate s) `shouldBe` gs "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"


    describe "leastAndMostCommonCounter" $ do
        it "tells the quantity of the least and most common elements in a template" $ do
            leastAndMostCommonCounter (afterStepCounter "NNCB" sample 10) `shouldBe` ((161,'H'),(1749,'B'))

        it "should solve the puzzle" $ do
            leastAndMostCommonCounter (afterStepCounter initial puzzle 10) `shouldBe` ((706,'N'),(3296,'F'))
            leastAndMostCommonCounter (afterStepCounter initial puzzle 40) `shouldBe` ((802701318456,'N'),(3678366520894,'F'))
            3678366520894 - 802701318456 `shouldBe` 2875665202438

