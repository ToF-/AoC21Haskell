module P12Spec
    where

import Test.Hspec
import P12
import qualified Data.Map as M

sample = ["start-A"
         ,"start-b"
         ,"A-c"
         ,"A-b"
         ,"b-d"
         ,"A-end"
         ,"b-end"]

g = graph sample
spec :: SpecWith ()
spec = do
    describe "graph" $ do
        it "create a graph from a list of connections" $ do
            M.toList g `shouldBe` [(Small "b",[Small "d",Small "end"])
                                  ,(Small "start",[Big "A",Small "b"])
                                  ,(Big "A",[Small "c",Small "b",Small "end"])]
    describe "adjacents" $ do
        it "tell what connections are possible from a cave, given already visited caves" $ do
            adjacents g (Small "start") [] `shouldBe` [Big "A", Small "b"]
            adjacents g (Big "A") [Small "b"] `shouldBe` [Small "c", Small "end"]

    describe "paths" $ do
        it "tell what possible paths are possible from a cave, given already visited caves" $ do
            possiblePaths g (Small "start") [] `shouldBe`
                [([Small "start"], [Small "start", Big "A"])
                ,([Small "start"], [Small "start", Small "b"])]

