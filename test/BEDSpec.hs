{-# LANGUAGE OverloadedLists #-}
module BEDSpec where

import Test.Hspec
import Data.Map
import Text.Parsec.ByteString
import Bio.Motions.BED
import Data.Either(isLeft)

test :: Spec
test = context "when parsing BED file" $ do
    let chrInfos = [("chr1", 50), ("chr2", 40), ("super_chromosome", 20), ("bajtAlina", 7)]
    let chrNums = fromList $ (fst <$> chrInfos) `zip` [0..]
    it "should parse file without errors" $ do
        ansOK <- parseFromFile (parseBED chrNums 1) "test/testdata/1.bed"
        ansOK `shouldBe` Right
            [ BindingSiteInfo {bsChain = 0, bsFrom = 1,  bsTo = 9,  bsType = 1}
            , BindingSiteInfo {bsChain = 1, bsFrom = 10, bsTo = 20, bsType = 1}
            , BindingSiteInfo {bsChain = 2, bsFrom = 5,  bsTo = 18, bsType = 1}
            ]

    it "should not parse file with errors" $ do
        ansErr <- parseFromFile (parseBED chrNums 1) "test/testdata/wrong.bed"
        ansErr `shouldSatisfy` isLeft

    context "when creating a lists of EnergyVectors" $
        it "should calculate energy vectors correctly" $ do
            ans <- parseBEDs 10 chrInfos ["test/testdata/0.bed", "test/testdata/1.bed"]
            ans `shouldBe` [ [[0, 1], [1, 0], [2, 0], [1, 0], [1, 0]]
                           , [[0, 0], [1, 1], [1, 1], [0, 0]]
                           , [[0, 1], [1, 1]]
                           , [[0, 0]]
                           ]

    it "should throw an exception when getting an invalid file" $
        let ansErr = parseBEDs 10 chrInfos ["test/testdata/wrong.bed"] in
        ansErr `shouldThrow` anyIOException

spec :: Spec
spec = describe "BED parser" test
