module P15Spec
    where

import Test.Hspec
import P15
import Puzzle15Data
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Heap as H
import qualified Data.List as L

sample = ["1163751742"
         ,"1381373672"
         ,"2136511328"
         ,"3694931569"
         ,"7463417111"
         ,"1319128137"
         ,"1359912421"
         ,"3125421639"
         ,"1293138521"
         ,"2311944581"]

c = cave sample

spec :: SpecWith ()
spec = do
    describe "a cave initially" $ do
        it "has a maxX and a maxY" $ do
            maxX c `shouldBe` 9 
            maxY c `shouldBe` 9 
        it "has its start node visited" $ do
            S.toList (visited c) `shouldBe` [(0,0)] 

        it "has all its node set to infinite risk" $ do
            M.lookup (3,3) (risks c) `shouldBe` Just (Node {risk = 4, totalRisk = 1000000, predecessor = Nothing})

        it "except for start node which is at 0 risk" $ do
            M.lookup (0,0) (risks c) `shouldBe` Just (Node {risk = 0, totalRisk = 0, predecessor = Nothing})

        it "has to visit start node" $ do
            toVisit c `shouldBe` H.fromList [(0,(0,0))]

    describe "neighbors" $ do
        it "tell neighbors from a coord for a given cave" $ do
            neighbors c (3,3) `shouldBe` [(2,3),(4,3),(3,2),(3,4)] 
            neighbors c (0,0) `shouldBe` [(1,0),(0,1)]
            neighbors c (9,9) `shouldBe` [(8,9),(9,8)]
        it "exclude already visited neighbors" $ do
            neighbors c (1,0) `shouldBe` [(2,0),(1,1)] 

    describe "visit cave" $ do
        it "affect risks of neighbors to node that was to visit" $ do
            let d = visit c
            M.lookup (0,1) (risks d) `shouldBe` Just (Node { risk = 1, totalRisk = 1, predecessor = Just (0,0) }) 
            M.lookup (1,0) (risks d) `shouldBe` Just (Node { risk = 1, totalRisk = 1, predecessor = Just (0,0) }) 
        it "prepare next nodes to visit" $ do
            let d = visit c
            let e = visit d
            let f = visit e
            toVisit d `shouldBe` H.fromList [(1,(1,0)),(1,(0,1))]
            toVisit e `shouldBe` H.fromList [(1,(0,1)),(4,(1,1)),(3,(2,0))]
            toVisit f `shouldBe` H.fromList [(3,(2,0)),(4,(1,1)),(7,(0,2))]

    describe "visit all" $ do
        let z = visitAll c
        it "visit the cave until no more nodes to visit" $ do
            toVisit z `shouldBe` H.empty 
        it "has all its node visited" $ do
            M.lookup (9,9) (risks z) `shouldBe` Just (Node {risk = 1, totalRisk = 40, predecessor = Just (8,9)})
        it "should pass the sample" $ do
            let target = (maxX c, maxY c)
            totalRisk <$> (M.lookup target (risks z)) `shouldBe` Just 40

        it "should pass the puzzle" $ do
            let c = cave puzzle
            let z = visitAll c
            let target = (maxX c, maxY c)
            totalRisk <$> (M.lookup target (risks z)) `shouldBe` Just 702
