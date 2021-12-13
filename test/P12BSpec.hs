module P12BSpec
    where

import Test.Hspec
import Data.List
import P12B
import qualified Data.Map as M

sample = ["start-A"
         ,"start-b"
         ,"A-c"
         ,"A-b"
         ,"b-d"
         ,"A-end"
         ,"b-end"]

puzzle = ["yb-pi"
         ,"jg-ej"
         ,"yb-KN"
         ,"LD-start"
         ,"end-UF"
         ,"UF-yb"
         ,"yb-xd"
         ,"qx-yb"
         ,"xd-end"
         ,"jg-KN"
         ,"start-qx"
         ,"start-ej"
         ,"qx-LD"
         ,"jg-LD"
         ,"xd-LD"
         ,"ej-qx"
         ,"end-KN"
         ,"DM-xd"
         ,"jg-yb"
         ,"ej-LD"
         ,"qx-UF"
         ,"UF-jg"
         ,"qx-jg"
         ,"xd-UF"]

g = graph sample
spec :: SpecWith ()
spec = do
    describe "graph" $ do
        it "create a graph from a list of connections" $ do
            M.toList g `shouldBe` [(Small "b",[Small "d",Small "end",Small "start",Big "A"])
                                  ,(Small "c",[Big "A"])
                                  ,(Small "d",[Small "b"])
                                  ,(Small "end",[Big "A",Small "b"])
                                  ,(Small "start",[Big "A",Small "b"])
                                  ,(Big "A",[Small "c",Small "b",Small "end",Small "start"])]
    describe "visitedTwice" $ do
        it "tell a possible cave that was visited twice in a path" $ do
            visitedTwice [Small "start"] `shouldBe` Nothing  
            visitedTwice [Small "c",Big "A",Small "c"] `shouldBe` (Just (Small "c"))

    describe "continue" $ do
        it "tell what possible paths are possible from the last cave of a path" $ do
            continue g [Small "start"] `shouldBe` [[Big "A", Small "start"]
                                                  ,[Small "b", Small "start"]]
            continue g [Small "b", Small "start"] `shouldBe` [[Small "d",Small "b",Small "start"]
                                                             ,[Small "end",Small "b",Small "start"]
                                                             ,[Big "A",Small "b",Small "start"]]

            continue g [Small "d", Small "b", Small "start"] `shouldBe` [[Small "b",Small "d",Small "b",Small "start"]]
            visitedTwice [Small "b",Small "d", Small "b", Small "start"]  `shouldBe` Just (Small "b") 
            continue g [Small "b",Small "d", Small "b", Small "start"] `shouldBe`
                [[Small "end",Small "b",Small "d",Small "b",Small "start"]
                ,[Big "A",Small "b",Small "d",Small "b",Small "start"]]

    describe "continuePaths" $ do
        it "continue paths given a list of paths" $ do
            let ps = [[Small "c",Big "A",Small "start"]
                     ,[Small "b",Big "A",Small "start"]
                     ,[Small "end",Big "A",Small "start"]
                     ,[Small "d",Small "b",Small "start"]
                     ,[Small "end",Small "b",Small "start"]
                     ,[Big "A",Small "b",Small "start"]]
            continuePaths g ps `shouldBe`
                [[Big "A",Small "c",Big "A",Small "start"]
                ,[Small "d",Small "b",Big "A",Small "start"]
                ,[Small "end",Small "b",Big "A",Small "start"]
                ,[Big "A",Small "b",Big "A",Small "start"]
                ,[Small "b",Small "d",Small "b",Small "start"]
                ,[Small "c",Big "A",Small "b",Small "start"]
                ,[Small "b",Big "A",Small "b",Small "start"]
                ,[Small "end",Big "A",Small "b",Small "start"]]

    describe "solutions" $ do 
        it "should pass the sample" $ do
            length (solutions g) `shouldBe` 36
        it "should pass the puzzle" $ do
            length (solutions (graph puzzle)) `shouldBe` 108035

