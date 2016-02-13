{-# LANGUAGE OverloadedLists #-}
module BEDSpec where

import Test.Hspec
import Bio.Motions.BED
import Bio.Motions.Types
import Text.ParserCombinators.Parsec
import Data.Either(isLeft)
import qualified Data.Vector.Unboxed as U

test :: Spec
test = do
  context "when parsing BED file" $ do
    ansOK <- runIO $ parseFromFile (parseBED 1) "test/testdata/1.bed"
    it "should parse file without errors" $
      ansOK `shouldBe` Right [
        BindingSiteInfo {bsChain = 0, bsFrom = 1, bsTo = 9, bsType = 1},
        BindingSiteInfo {bsChain = 1, bsFrom = 10, bsTo = 20, bsType = 1},
        BindingSiteInfo {bsChain = 2, bsFrom = 5, bsTo = 18, bsType = 1}]
    ansErr <- runIO $ parseFromFile (parseBED 1) "test/testdata/wrong.bed"
    it "should not parse file with errors" $
      ansErr `shouldSatisfy` isLeft
  context "when creating a lists of EnergyVectors" $ do
    ans <- runIO $ parseBEDs 10 [50, 40, 20] ["test/testdata/0.bed", "test/testdata/1.bed"]
    it "should calculate energy vectors correctly" $
      ans `shouldBe` [
        [[0,1],[1,0],[2,0],[1,0],[1,0]],
        [[0,0],[1,1],[1,1],[0,0]],
        [[0,1],[1,1]]]
    let ansErr = parseBEDs 10 [50, 40, 20] ["test/testdata/wrong.bed"]
    it "should throw an exception when getting an invalid file" $
      ansErr `shouldThrow` anyIOException

spec :: Spec
spec = describe "BED parser" test
