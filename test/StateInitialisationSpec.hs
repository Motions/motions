{-# LANGUAGE OverloadedLists #-}
module StateInitialisationSpec where

import Data.Maybe
import Test.Hspec
import Control.Lens
import Control.Monad.Random
import qualified Data.Map.Strict as M

import Bio.Motions.Common
import Bio.Motions.StateInitialisation
import Bio.Motions.Types
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Common


spec :: Spec
spec = context "when generating state" $ do
    let ans = run (initialise 1000 10 12 3 fewShortEVectors) 123
    it "should generate something, when possible" $
        ans `shouldSatisfy` isJust
    it "should generate nothing, when impossible" $
        run (initialise 1000 100 3 3 oneSuperLongEVector) 123 `shouldSatisfy` isNothing
    let Just dump = ans
    it "should generate proper amount of non-lamin binders" $
        length (dumpBinders dump) - length (getAllLamins dump) `shouldBe` 12
    it "should generate beads with proper energy vectors" $
        map (map dumpBeadEV) (dumpChains dump) `shouldBe` fewShortEVectors
    let ansSpace = dumpToSpace dump
    let beadsPositions = map (map dumpBeadPosition) $ dumpChains dump
    it "should generate non-intersecting chains" $
        beadsPositions `shouldNotSatisfy` any (\chain ->
            or $ zipWith (intersectsChain ansSpace) chain (tail chain))
    it "should generate atoms on distinct fields" $
        M.size ansSpace `shouldBe` length (concat $ dumpChains dump) + length (dumpBinders dump)

run :: Rand StdGen (Maybe Dump) -> Int -> Maybe Dump
run m seed = evalRand m $ mkStdGen seed

getAllLamins :: Dump -> [BinderInfo]
getAllLamins d = filter (\x -> x ^. binderType == laminType) $ dumpBinders d

dumpToSpace :: Dump -> Space
dumpToSpace dump = M.fromList $ [(b ^. position, asAtom b) | b <- dumpBinders dump] ++
                                [(b ^. position, asAtom b) | b <- concat $ addIndices $ dumpChains dump]

fewShortEVectors :: [[EnergyVector]]
fewShortEVectors = [[[0,1], [1,0], [2,0], [1,0], [1,0]],
                    [[0,0], [1,1], [1,1], [0,0]],
                    [[0,1], [1,1], [1,1], [0,0]]]

oneSuperLongEVector :: [[EnergyVector]]
oneSuperLongEVector = [replicate 100000 [1,1,1]]
