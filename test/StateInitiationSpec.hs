module StateInitiationSpec where

import Data.Maybe
import Test.Hspec
import Control.Monad.Random.Class
import Control.Monad.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as M

import Bio.Motions.StateInitiation
import Bio.Motions.Types
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Common


spec :: Spec
spec = context "when generating state" $ do
    let ans = run (initialize 10000 10 12 3 fewShortEVectors) 123
    it "should generate something, when possible" $
        ans `shouldSatisfy` isJust
    it "should generate nothing, when impossible" $
        run (initialize 1000 100 3 3 oneSuperLongEVector) 123 `shouldSatisfy` isNothing
    let Just dump = ans
    it "should generate proper amount of non-lamin binders" $
        length (dumpBinders dump) - length (getAllLamins dump) `shouldBe` 12
    it "should generate beads with proper energy vectors" $
        map (map dumpBeadEV) (dumpChains dump) `shouldBe` fewShortEVectors
    let ans_space = dumpToSpace dump
    let beads_positions = map (map dumpBeadPosition) $ dumpChains dump
    it "should generate non-intersecting chains" $
        beads_positions `shouldNotSatisfy` any (\chain ->
            or $ zipWith (intersectsChain ans_space) chain (tail chain))
    it "should generate atoms on distinct fields" $
        M.size ans_space `shouldBe` length (concat $ dumpChains dump) + length (dumpBinders dump)

run :: Rand StdGen (Maybe Dump) -> Int -> Maybe Dump
run m seed = evalRand m $ mkStdGen seed

getAllLamins :: Dump -> [BinderInfo]
getAllLamins d = filter (\x -> binderType x == BinderType 0) $ dumpBinders d

toEnergyVectors :: [[[Int]]] -> [[EnergyVector]]
toEnergyVectors = map (map $ EnergyVector . U.fromList)

dumpToSpace :: Dump -> Space
dumpToSpace dump = M.fromList $ [(binderPosition b, Binder b) | b <- dumpBinders dump] ++
                                [(beadPosition b, Bead b) | b <- concat $ addIndices $ dumpChains dump]


fewShortEVectors :: [[EnergyVector]]
fewShortEVectors = toEnergyVectors [[[0,1], [1,0], [2,0], [1,0], [1,0]],
                                   [[0,0], [1,1], [1,1], [0,0]],
                                   [[0,1], [1,1], [1,1], [0,0]]]

oneSuperLongEVector :: [[EnergyVector]]
oneSuperLongEVector = toEnergyVectors [replicate 100000 [1,1,1]]
