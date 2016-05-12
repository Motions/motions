{-# LANGUAGE OverloadedLists #-}
module BEDSpec where

import Test.Hspec
import Data.Bimap
import Bio.Motions.BED
import Data.Either(isLeft)
import Control.Monad.State.Strict
import Control.Monad.Except

test :: Spec
test = context "when parsing BED file" $ do
    let chrInfos = [("chr1", 50), ("chr2", 40), ("super_chromosome", 20), ("bajtAlina", 7)]
    let chrNums = fromList $ (fst <$> chrInfos) `zip` [0..]
    it "should parse file without errors" $ do
        ansOK <- runExceptT $ (runStateT $ parseChrFromFile 1 "test/testdata/1.bed") chrNums
        fst <$> ansOK `shouldBe` Right
            [ BindingSiteInfo {bsChain = 0, bsFrom = 1,  bsTo = 9,  bsType = 1}
            , BindingSiteInfo {bsChain = 1, bsFrom = 10, bsTo = 20, bsType = 1}
            , BindingSiteInfo {bsChain = 2, bsFrom = 5,  bsTo = 18, bsType = 1}
            ]

    it "should not parse file with errors" $ do
        ansErr <- runExceptT $ (runStateT $ parseChrFromFile 1 "test/testdata/wrong.bed") chrNums
        ansErr `shouldSatisfy` isLeft

    context "when creating a lists of EnergyVectors" $
        it "should calculate energy vectors correctly" $ do
            ans <- parseBEDs 10 chrInfos ["test/testdata/0.bed", "test/testdata/1.bed"]
            fst ans `shouldBe` [ [[0, 1], [1, 0], [2, 0], [1, 0], [1, 0]]
                           , [[0, 0], [1, 1], [1, 1], [0, 0]]
                           , [[0, 1], [1, 1]]
                           , [[0, 0]]
                           ]
            snd ans `shouldBe` fst <$> chrInfos

    it "should throw an exception when getting an invalid file" $
        let ansErr = parseBEDs 10 chrInfos ["test/testdata/wrong.bed"] in
        ansErr `shouldThrow` anyIOException

spec :: Spec
spec = describe "BED parser" test
