module P18Spec
    where

import Test.Hspec
import P18
import Puzzle18Data

mySn = sn "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"

spec :: SpecWith ()
spec = do
    describe "snailfish number" $ do
        it "is created from a string" $ do
            sn "[1,2]" `shouldBe` 
                Pair 1 (Number 2 1, Number 3 2)

            sn "[[1,2],3]" `shouldBe` 
                Pair 1 (Pair 2 (Number 4 1, Number 5 2)
                       ,Number 3 3)

            sn "[[1,9],[8,5]]" `shouldBe` 
                Pair 1 (Pair 2 (Number 4 1,Number 5 9),Pair 3 (Number 6 8,Number 7 5))

    describe "number snailfish" $ do
        it "is representing number as a string" $ do
            ns (Pair 1 (Pair 2 (Number 4 1,Number 5 9),Pair 3 (Number 6 8,Number 7 5)))
                 `shouldBe` "[[1,9],[8,5]]"

    describe "next left index" $ do
        it "tell the index of the next regular number on the left of a regular number" $ do
            nextLeftIx 28 mySn `shouldBe` Just 27
            nextLeftIx 16 mySn `shouldBe` Nothing 

    describe "next right index" $ do
        it "tell the index of the next regular number on the right of a regular number" $ do
            nextRightIx 28 mySn `shouldBe` Just 29
            nextRightIx 31 mySn `shouldBe` Nothing 


    describe "replace index" $ do
        it "replace an element in a number" $ do
            let myNewSn = replaceIx mySn 25 7 
            myNewSn `shouldBe` 
                Pair 1 
                    (Pair 2 
                        (Pair 4 
                            (Pair 8 
                                (Number 16 1,Number 17 3)
                            ,Pair 9 
                                (Number 18 5,Number 19 3)
                            )
                        ,Pair 5 
                            (Pair 10 
                                (Number 20 1,Number 21 3)
                            ,Pair 11 
                                (Number 22 8,Number 23 7)
                            )
                        )
                    ,Pair 3 
                        (Pair 6 
                            (Pair 12 
                                (Number 24 4,Number 25 7)
                            ,Pair 13 
                                (Number 26 6,Number 27 9)
                            )
                        ,Pair 7 
                            (Pair 14 
                                (Number 28 8,Number 29 2)
                            ,Pair 15 
                                (Number 30 7,Number 31 3)
                    )))
    describe "add index" $ do
        it "add a value to an element in a number" $ do
            let myNewSn = addIx mySn 17 4 
            myNewSn `shouldBe` 
                Pair 1 
                    (Pair 2 
                        (Pair 4 
                            (Pair 8 
                                (Number 16 1,Number 17 7)
                            ,Pair 9 
                                (Number 18 5,Number 19 3)
                            )
                        ,Pair 5 
                            (Pair 10 
                                (Number 20 1,Number 21 3)
                            ,Pair 11 
                                (Number 22 8,Number 23 7)
                            )
                        )
                    ,Pair 3 
                        (Pair 6 
                            (Pair 12 
                                (Number 24 4,Number 25 9)
                            ,Pair 13 
                                (Number 26 6,Number 27 9)
                            )
                        ,Pair 7 
                            (Pair 14 
                                (Number 28 8,Number 29 2)
                            ,Pair 15 
                                (Number 30 7,Number 31 3)
                    )))


    describe "nested" $ do
        it "find the pair that is nested in at least four pairs" $ do
            let n = sn "[[[[[9,8],1],2],3],4]"
            nested n `shouldBe` Just (Pair 16 (Number 32 9,Number 33 8))
            let n = sn "[7,[6,[5,[4,[3,2]:]]]]"
            nested n `shouldBe` Just (Pair 31 (Number 62 3,Number 63 2))
            let n = sn "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
            nested n `shouldBe` Just (Pair 23 (Number 46 7,Number 47 3))
            let n = sn "[[[[[[1,2],3],4],5],6],7]"
            nested n `shouldBe` Just (Pair 32 (Number 64 1,Number 65 2))

    describe "explodePairIx" $ do
        it "explodes a pair from a number" $ do
            let n = sn "[[[[[9,8],1],2],3],4]"
            let (Just p) = nested n 
            let s = ns (explodePairIx p n)
            s `shouldBe` "[[[[0,9],2],3],4]" 

    describe "explodeOne" $ do
        it "explode a nested pair from a number" $ do
            let ex = ns . explodeOne . sn
            ex "[4,6]" `shouldBe` "[4,6]" 
            ex "[[[[[9,8],1],2],3],4]" `shouldBe` "[[[[0,9],2],3],4]"
            ex "[[6,[5,[4,[3,2]]]],1]"  `shouldBe`  "[[6,[5,[7,0]]],3]"
            ex "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"  `shouldBe`  "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
            ex "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"  `shouldBe` "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

    describe "explode" $ do
        it "explode all nestedPairs from a number" $ do
            let exa = ns . explode . sn
            exa "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" `shouldBe` "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
            exa "[[[[[[[1,2],3],4],5],6],7],8]" `shouldBe` "[[[[0,14],6],7],8]"

    describe "split" $ do
        it "splits a element greater than 9" $ do
            let sp = ns . split . sn
            sp "[4,5]" `shouldBe` "[4,5]" 
            sp "10" `shouldBe` "[5,5]" 
            sp "[15,23]" `shouldBe`"[[7,8],23]"
            sp "[5,23]" `shouldBe` "[5,[11,12]]"
            sp "[5,[11,12]]" `shouldBe` "[5,[[5,6],12]]"
            sp "[5,[[5,6],12]]" `shouldBe` "[5,[[5,6],[6,6]]]"

    describe "reduce" $ do
        it "explodes and split a number" $ do
            let re = ns . reduce . sn
            re "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" `shouldBe` "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" 

    describe "add" $ do
        it "create a pair with added elements" $ do
            let a = sn "[4,2]"
            let b = sn "[3,6]"
            add a b `shouldBe` sn "[[4,2],[3,6]]"
            let ad x y = ns (add (sn x) (sn y))
            ad "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]" `shouldBe` "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
            ad "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                 `shouldBe` "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"


    describe "addList" $ do
        it "add all numbers" $ do
            let input = ["[1,1]"
                        ,"[2,2]"
                        ,"[3,3]"
                        ,"[4,4]"]
            let sns = map sn input
            ns (addAll sns)  `shouldBe` "[[[[1,1],[2,2]],[3,3]],[4,4]]"

            let input = ["[1,1]"
                        ,"[2,2]"
                        ,"[3,3]"
                        ,"[4,4]"
                        ,"[5,5]"]
            let sns = map sn input
            ns (addAll sns)  `shouldBe` "[[[[3,0],[5,3]],[4,4]],[5,5]]"

            let input = ["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                        ,"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                        ,"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                        ,"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                        ,"[7,[5,[[3,8],[1,4]]]]"
                        ,"[[2,[2,2]],[8,[8,1]]]"
                        ,"[2,9]"
                        ,"[1,[[[9,3],9],[[9,0],[0,7]]]]"
                        ,"[[[5,[7,4]],7],1]"
                        ,"[[[[4,2],2],6],[8,7]]"]
          
            let sns = map sn input
            ns (addAll sns)  `shouldBe` "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

    describe "magnitude" $ do
        it "equals 3 times the magnitude of left element + 2 times the magnitude of right element" $ do
            magnitude (sn "[9,1]") `shouldBe` 29 
            magnitude (sn "[[9,1],[1,9]]") `shouldBe` 129 
            magnitude (sn "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")  `shouldBe`  3488

            let input = ["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
                        ,"[[[5,[2,8]],4],[5,[[9,9],0]]]"
                        ,"[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
                        ,"[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
                        ,"[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
                        ,"[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
                        ,"[[[[5,4],[7,7]],8],[[8,3],8]]"
                        ,"[[9,3],[[9,9],[6,[4,9]]]]"
                        ,"[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                        ,"[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"]
            let sns = map sn input
            magnitude (addAll sns) `shouldBe` 4140

        it "should solve part A of the puzzle" $ do
            let sns = map sn puzzle
            magnitude (addAll sns) `shouldBe` 4057

    describe "largest" $ do
        it "tell largest magnitude of any two numbers" $ do

            let input = ["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
                        ,"[[[5,[2,8]],4],[5,[[9,9],0]]]"
                        ,"[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
                        ,"[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
                        ,"[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
                        ,"[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
                        ,"[[[[5,4],[7,7]],8],[[8,3],8]]"
                        ,"[[9,3],[[9,9],[6,[4,9]]]]"
                        ,"[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                        ,"[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"]
            let sns = map sn input
            largest sns `shouldBe` 3993
        it "should solve part B of the puzzle" $ do
            let sns = map sn puzzle
            largest sns `shouldBe` 4683

