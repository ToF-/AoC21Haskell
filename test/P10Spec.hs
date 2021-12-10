module P10Spec
    where

import Test.Hspec
import P10

sample = ["[({(<(())[]>[[{[]{<()<>>"
         ,"[(()[<>])]({[<{<<[]>>("
         ,"{([(<{}[<>[]}>{[]{[(<()>"
         ,"(((({<>}<{<{<>}{[]{[]{}"
         ,"[[<[([]))<([[{}[[()]]]"
         ,"[{[{({}]{}}([{[{{{}}([]"
         ,"{<[[]]>}<{[{[{[]{()[[[]"
         ,"[<(<(<(<{}))><([]([]()"
         ,"<{([([[(<>()){}]>(<<{{"
         ,"<{([{{}}[<[[[<>{}]]]>[]]"]

spec :: SpecWith ()
spec = do
    describe "find first illegal char" $ do
        it "finds the first illegal closer in a line" $ do
            findFirstIllegalChar "[({(<(())[]>[[{[]{<()<>>" `shouldBe` Nothing
            findFirstIllegalChar " {([(<{}[<>[]}>{[]{[(<()>" `shouldBe` Just '}'

    describe "total syntax error score" $ do
        it "should solve the sample" $ do
            totalSyntaxErrorScore sample `shouldBe` 26397

        it "should solve the puzzle" $ do
            puzzle <- lines <$> readFile "test/Puzzle10Data.txt"
            totalSyntaxErrorScore puzzle `shouldBe` 369105

    describe "completion" $ do
        it "tells the chars that should complete an incomplete chunk" $ do
            completion "[({(<(())[]>[[{[]{<()<>>" `shouldBe` "}}]])})]"

    describe "completions" $ do
        it "tells the completions for all incomplete chunks" $ do
            completions sample  `shouldBe` ["}}]])})]"
                                           ,")}>]})"
                                           ,"}}>}>))))"
                                           ,"]]}}]}]}>"
                                           ,"])}>"]

    describe "completion score" $ do
        it "gives the score of a completion" $ do
            completionScore "}}]])})]" `shouldBe` 288957

    describe "winner" $ do
        it "should solve the sample" $ do
            winner sample `shouldBe` 288957

        it "should solve the puzzle" $ do
            puzzle <- lines <$> readFile "test/Puzzle10Data.txt"
            winner puzzle `shouldBe` 3999363569
