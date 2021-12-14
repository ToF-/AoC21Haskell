module P14Spec
    where

import Test.Hspec
import P14


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

initial= "OHFNNCKCVOBHSSHONBNF"

puzzle = [("SV",'O')
         ,("KP",'H')
         ,("FP",'B')
         ,("VP",'V')
         ,("KN",'S')
         ,("KS",'O')
         ,("SB",'K')
         ,("BS",'K')
         ,("OF",'O')
         ,("ON",'S')
         ,("VS",'F')
         ,("CK",'C')
         ,("FB",'K')
         ,("CH",'K')
         ,("HS",'H')
         ,("PO",'F')
         ,("NP",'N')
         ,("FH",'C')
         ,("FO",'O')
         ,("FF",'C')
         ,("CO",'K')
         ,("NB",'V')
         ,("PP",'S')
         ,("BB",'N')
         ,("HH",'B')
         ,("KK",'H')
         ,("OP",'K')
         ,("OS",'V')
         ,("KV",'F')
         ,("VH",'F')
         ,("OB",'S')
         ,("CN",'H')
         ,("SF",'K')
         ,("SN",'P')
         ,("NF",'H')
         ,("HB",'V')
         ,("VC",'S')
         ,("PS",'P')
         ,("NK",'B')
         ,("CV",'P')
         ,("BC",'S')
         ,("NH",'K')
         ,("FN",'P')
         ,("SH",'F')
         ,("FK",'P')
         ,("CS",'O')
         ,("VV",'H')
         ,("OC",'F')
         ,("CC",'N')
         ,("HK",'N')
         ,("FS",'P')
         ,("VF",'B')
         ,("SS",'V')
         ,("PV",'V')
         ,("BF",'V')
         ,("OV",'C')
         ,("HO",'F')
         ,("NC",'F')
         ,("BN",'F')
         ,("HC",'N')
         ,("KO",'P')
         ,("KH",'F')
         ,("BV",'S')
         ,("SK",'F')
         ,("SC",'F')
         ,("VN",'V')
         ,("VB",'V')
         ,("BH",'O')
         ,("CP",'K')
         ,("PK",'K')
         ,("PB",'K')
         ,("FV",'S')
         ,("HN",'K')
         ,("PH",'B')
         ,("VK",'B')
         ,("PC",'H')
         ,("BO",'H')
         ,("SP",'V')
         ,("NS",'B')
         ,("OH",'N')
         ,("KC",'H')
         ,("HV",'F')
         ,("HF",'B')
         ,("HP",'S')
         ,("CB",'P')
         ,("PN",'S')
         ,("BK",'K')
         ,("PF",'N')
         ,("SO",'P')
         ,("CF",'B')
         ,("VO",'C')
         ,("OO",'K')
         ,("FC",'F')
         ,("NV",'F')
         ,("OK",'K')
         ,("NN",'O')
         ,("NO",'O')
         ,("BP",'O')
         ,("KB",'O')
         ,("KF",'O')]
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


