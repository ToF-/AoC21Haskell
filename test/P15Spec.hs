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

extendedSample = ["11637517422274862853338597396444961841755517295286"
                 ,"13813736722492484783351359589446246169155735727126"
                 ,"21365113283247622439435873354154698446526571955763"
                 ,"36949315694715142671582625378269373648937148475914"
                 ,"74634171118574528222968563933317967414442817852555"
                 ,"13191281372421239248353234135946434524615754563572"
                 ,"13599124212461123532357223464346833457545794456865"
                 ,"31254216394236532741534764385264587549637569865174"
                 ,"12931385212314249632342535174345364628545647573965"
                 ,"23119445813422155692453326671356443778246755488935"
                 ,"22748628533385973964449618417555172952866628316397"
                 ,"24924847833513595894462461691557357271266846838237"
                 ,"32476224394358733541546984465265719557637682166874"
                 ,"47151426715826253782693736489371484759148259586125"
                 ,"85745282229685639333179674144428178525553928963666"
                 ,"24212392483532341359464345246157545635726865674683"
                 ,"24611235323572234643468334575457944568656815567976"
                 ,"42365327415347643852645875496375698651748671976285"
                 ,"23142496323425351743453646285456475739656758684176"
                 ,"34221556924533266713564437782467554889357866599146"
                 ,"33859739644496184175551729528666283163977739427418"
                 ,"35135958944624616915573572712668468382377957949348"
                 ,"43587335415469844652657195576376821668748793277985"
                 ,"58262537826937364893714847591482595861259361697236"
                 ,"96856393331796741444281785255539289636664139174777"
                 ,"35323413594643452461575456357268656746837976785794"
                 ,"35722346434683345754579445686568155679767926678187"
                 ,"53476438526458754963756986517486719762859782187396"
                 ,"34253517434536462854564757396567586841767869795287"
                 ,"45332667135644377824675548893578665991468977611257"
                 ,"44961841755517295286662831639777394274188841538529"
                 ,"46246169155735727126684683823779579493488168151459"
                 ,"54698446526571955763768216687487932779859814388196"
                 ,"69373648937148475914825958612593616972361472718347"
                 ,"17967414442817852555392896366641391747775241285888"
                 ,"46434524615754563572686567468379767857948187896815"
                 ,"46833457545794456865681556797679266781878137789298"
                 ,"64587549637569865174867197628597821873961893298417"
                 ,"45364628545647573965675868417678697952878971816398"
                 ,"56443778246755488935786659914689776112579188722368"
                 ,"55172952866628316397773942741888415385299952649631"
                 ,"57357271266846838237795794934881681514599279262561"
                 ,"65719557637682166874879327798598143881961925499217"
                 ,"71484759148259586125936169723614727183472583829458"
                 ,"28178525553928963666413917477752412858886352396999"
                 ,"57545635726865674683797678579481878968159298917926"
                 ,"57944568656815567976792667818781377892989248891319"
                 ,"75698651748671976285978218739618932984172914319528"
                 ,"56475739656758684176786979528789718163989182927419"
                 ,"67554889357866599146897761125791887223681299833479"]

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

    describe "extended" $ do
        it "extends 4 times a map" $ do
            extended sample `shouldBe` extendedSample 
        it "should pass the sample extended" $ do
            let c = cave (extended sample)
            let z = visitAll c
            let target = (maxX c, maxY c)
            totalRisk <$> (M.lookup target (risks z)) `shouldBe` Just 315


        it "should pass the puzzle extended" $ do
            let c = cave (extended puzzle)
            let z = visitAll c
            let target = (maxX c, maxY c)
            totalRisk <$> (M.lookup target (risks z)) `shouldBe` Just 2955
