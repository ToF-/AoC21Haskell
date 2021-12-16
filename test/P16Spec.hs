module P16Spec
    where

import Test.Hspec
import P16

spec :: SpecWith ()
spec = do
    describe "hexToBinary" $ do
        it "convert a hex string into a binary value" $ do
            hexToBinary "0123456789ABCDEF" `shouldBe` 
                "0000000100100011010001010110011110001001101010111100110111101111"

    describe "fromBinary" $ do
        it "convert a binary value to integer" $ do
            fromBinary "11110000" `shouldBe` 240 


    describe "version" $ do
        it "tell the version of a packet" $ do
            version "110100101111111000101000" `shouldBe` (Version 6) 

    describe "type id" $ do
        it "tell the type id of a packet" $ do
            typeId "110100101111111000101000" `shouldBe` 4

    describe "literal value" $ do
        it "tell the value of a packet of type id 4" $ do
            value "110100101111111000101000" `shouldBe` Value 21 2021

    describe "length mode" $ do
        it "tell how to interpret subPackets length" $ do
            let b = hexToBinary "38006F45291200" 
            lengthMode b `shouldBe` TotalLength 27 

            let c = hexToBinary "EE00D40C823060"
            lengthMode c `shouldBe` NbSubPackets 3  

    describe "packet" $ do
        let b = hexToBinary "38006F45291200" 
        let c = hexToBinary "EE00D40C823060"
        let d = hexToBinary "8A004A801A8002F478"
        let e = hexToBinary "620080001611562C8802118E34"
        let f = hexToBinary "C0015000016115A2E0802F182340"
        let g = hexToBinary "A0016C880162017C3686B18A3D4780"

        it "interprets a binary as a packet" $ do
            packet "01010000001" `shouldBe` LiteralValue (Version 2)  (Value 11 1)
            packet "01010010001001000000000" `shouldBe` LiteralValue (Version 2) (Value 16 20) 

        it "can include packets" $ do
            packet b `shouldBe` 
               Operator (Version 1) (TotalLength 27) [LiteralValue (Version 6) (Value 11 10)
                                                     ,LiteralValue (Version 2) (Value 16 20)]
            
            packet c `shouldBe` 
                Operator (Version 7) (NbSubPackets 3) [LiteralValue (Version 2) (Value 11 1)
                                                      ,LiteralValue (Version 4) (Value 11 2)
                                                      ,LiteralValue (Version 1) (Value 11 3)]
            packet d `shouldBe` Operator (Version 4) (NbSubPackets 1)
                [Operator (Version 1) (NbSubPackets 1)
                    [Operator (Version 5) (TotalLength 11) 
                        [LiteralValue (Version 6) (Value 11 15)]]]

            packet e `shouldBe` Operator (Version 3) (NbSubPackets 2) 
                [Operator (Version 0) (TotalLength 22) 
                    [LiteralValue (Version 0) (Value 11 10),LiteralValue (Version 5) (Value 11 11)]
                    ,Operator (Version 1) (NbSubPackets 2) 
                        [LiteralValue (Version 0) (Value 11 12),LiteralValue (Version 3) (Value 11 13)]]
            
            packet f `shouldBe` Operator (Version 6) (TotalLength 84)
                [Operator (Version 0) (TotalLength 22) 
                    [LiteralValue (Version 0) (Value 11 10),LiteralValue (Version 6) (Value 11 11)]
                    ,Operator (Version 4) (NbSubPackets 2) 
                        [LiteralValue (Version 7) (Value 11 12),LiteralValue (Version 0) (Value 11 13)]]


            packet g `shouldBe` Operator (Version 5) (TotalLength 91)
                [Operator (Version 1) (NbSubPackets 1) 
                    [Operator (Version 3) (NbSubPackets 5) 
                        [LiteralValue (Version 7) (Value 11 6)
                        ,LiteralValue (Version 6) (Value 11 6)
                        ,LiteralValue (Version 5) (Value 11 12)
                        ,LiteralValue (Version 2) (Value 11 15)
                        ,LiteralValue (Version 2) (Value 11 15)]]]
    describe "versionSum" $ do
        it "tell the sum of all the versions inside a packet" $ do
            let a = hexToBinary "8A004A801A8002F478"
            let b = hexToBinary "620080001611562C8802118E34"
            let c = hexToBinary "C0015000016115A2E0802F182340"
            let d = hexToBinary "A0016C880162017C3686B18A3D4780"

            versionSum (packet a) `shouldBe` 16
            versionSum (packet b) `shouldBe` 12
            versionSum (packet c) `shouldBe` 23
            versionSum (packet d) `shouldBe` 31

        it "should pass the puzzle" $ do
            let puzzle = "A20D74AFC6C80CEA7002D4009202C7C00A6830029400F500218080C3002D006CC2018658056E7002DC00C600E75002ED6008EDC00D4003E24A13995080513FA309482649458A054C6E00E6008CEF204BA00B080311B21F4101006E1F414846401A55002F53E9525B845AA7A789F089402997AE3AFB1E6264D772D7345C6008D8026200E41D83B19C001088CB04A294ADD64C0129D818F802727FFF3500793FFF9A801A801539F42200DC3801A39C659ACD3FC6E97B4B1E7E94FC1F440219DAFB5BB1648E8821A4FF051801079C379F119AC58ECC011A005567A6572324D9AE6CCD003639ED7F8D33B8840A666B3C67B51388440193E003413A3733B85F2712DEBB59002B930F32A7D0688010096019375300565146801A194844826BB7132008024C8E4C1A69E66108000D39BAD950802B19839F005A56D9A554E74C08028992E95D802D2764D93B27900501340528A7301F2E0D326F274BCAB00F5009A737540916D9A9D1EA7BD849100425D9E3A9802B800D24F669E7691E19CFFE3AF280803440086C318230DCC01E8BF19E33980331D631C593005E80330919D718EFA0E3233AE31DF41C67F5CB5CAC002758D7355DD57277F6BF1864E9BED0F18031A95DDF99EB7CD64626EF54987AE007CCC3C4AE0174CDAD88E65F9094BC4025FB2B82C6295F04100109263E800FA41792BCED1CC3A233C86600B48FFF5E522D780120C9C3D89D8466EFEA019009C9600A880310BE0C47A100761345E85F2D7E4769240287E80272D3CEFF1C693A5A79DFE38D27CCCA75E5D00803039BFF11F401095F714657DC56300574010936491FBEC1D8A4402234E1E68026200CC5B8FF094401C89D12E14B803325DED2B6EA34CA248F2748834D0E18021339D4F962AB005E78AE75D08050E10066114368EE0008542684F0B40010B8AB10630180272E83C01998803104E14415100623E469821160"
            versionSum (packet (hexToBinary puzzle)) `shouldBe` 891
