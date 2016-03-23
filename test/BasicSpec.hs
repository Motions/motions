{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module BasicSpec where

import LoadTestCallbacks

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property (rejected)

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Chain.Internal
import Bio.Motions.Representation.Common
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius
import Bio.Motions.Callback.Parser.TH
import Bio.Motions.Representation.Dump

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Maybe
import Data.MonoTraversable
import Data.Proxy
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Linear

import GHC.TypeLits

x `shouldAlmostBe` y = abs (x - y) `shouldSatisfy` (< 1e-7)

instance MonadRandom m => MonadRandom (PropertyM m) where
    getRandom = lift getRandom
    getRandoms = lift getRandoms
    getRandomR = lift . getRandomR
    getRandomRs = lift . getRandomRs

testIntersectsChain :: Spec
testIntersectsChain = do
    it "reports actual intersections to exist" $
        intersectsChain space (V3 7 8 7) (V3 7 7 8) `shouldBe` True

    it "doesn't report about non-existing intersections" $
        intersectsChain space (V3 7 8 7) (V3 7 8 8) `shouldBe` False
  where
    space = [ (V3 7 7 7, Located (V3 7 7 7) $ BeadSig $ BeadSignature ev 0 0 0)
            , (V3 7 8 8, Located (V3 7 8 8) $ BeadSig $ BeadSignature ev 0 0 1)
            ]
    ev = []

testRepr :: _ => proxy repr -> Spec
testRepr (_ :: _ repr) = before (loadDump dump freezePredicate :: IO repr) $ do
    context "when redumping" $
        beforeWith makeDump testRedump

    context "when inspecting the data"
        testInspect

    context "when computing callbacks"
        testCallbacks

    context "when generating a move"
        testGenerateMove

    beforeWith (\repr -> fst <$> performMove beadMove repr) $
        context "after making a bead move" $ do
            testAfterBeadMove

            beforeWith (\repr -> fst <$> performMove binderMove repr) $
                context "after making a binder move"
                    testAfterBinderMove
  where
    beads = sum . map length $ dumpIndexedChains dump
    binders = length $ dumpBinders dump

    freezePredicate b = b ^. beadChain == 0
    dump = Dump
        { dumpBinders =
            [ BinderInfo (V3 0 1 2) bi0
            , BinderInfo (V3 0 1 3) bi0
            , BinderInfo (V3 5 5 5) bi1
            ]
        , dumpChains =
            [ [ DumpBeadInfo (V3 0 1 1) ev0
              , DumpBeadInfo (V3 5 6 6) ev1
              , DumpBeadInfo (V3 5 5 6) ev0
              ]
            , [ DumpBeadInfo (V3 0 0 2) ev0
              , DumpBeadInfo (V3 5 4 5) ev1
              ]
            , [ DumpBeadInfo (V3 7 7 7) ev0
              , DumpBeadInfo (V3 7 8 8) ev0
              ]
            ]
        }

    [bi0, bi1] = map BinderType [0, 1]
    (ev0, ev1) = ([1, 0], [0, 1000])

    complexFunctionResult = 45117.35291086203

    beadMove = Move (V3 5 6 6) (V3 0 0 (-1))

    binderMove = Move (V3 0 1 2) (V3 1 0 0)

    updatedChain = [ BeadInfo (V3 0 1 1) ev0 0 0 0
                   , BeadInfo (V3 5 6 5) ev1 1 0 1
                   , BeadInfo (V3 5 5 6) ev0 2 0 2
                   ]

    updatedChains = updatedChain : tail (dumpIndexedChains dump)

    updatedBinders = [ BinderInfo (V3 1 1 2) bi0
                     , BinderInfo (V3 0 1 3) bi0
                     , BinderInfo (V3 5 5 5) bi1
                     ]

    testRedump :: SpecWith Dump
    testRedump = do
        it "yields the same chains" $ \dump' ->
            dumpChains dump' `shouldBe` dumpChains dump

        it "yields the same binders" $ \dump' ->
            dumpBinders dump' `shouldMatchList` dumpBinders dump

    testInspect :: SpecWith repr
    testInspect = do
        it "yields the same number of chains" $
            getNumberOfChains >=> (`shouldBe` length (dumpChains dump))

        it "yields the same binders" $ \repr -> do
            binders <- getBinders repr (pure . otoList)
            binders `shouldBe` dumpBinders dump

        it "yields the same beads" $ \repr -> do
            beads <- forM [0..length (dumpChains dump) - 1] $
                \idx -> getChain repr idx (pure . otoList)
            beads `shouldBe` dumpIndexedChains dump

        context "when using getAtomAt" $ do
            it "returns binders" $ \repr ->
                forM_ (dumpBinders dump) $ \binder -> do
                    atom <- getAtomAt (binder ^. position) repr
                    atom `shouldBe` Just (asAtom binder)

            it "returns beads" $ \repr ->
                forM_ (concat $ dumpIndexedChains dump) $ \bead -> do
                    atom <- getAtomAt (bead ^. position) repr
                    atom `shouldBe` Just (asAtom bead)

            it "returns Nothing" $ \repr -> do
                atom <- getAtomAt (V3 0 0 0) repr
                atom `shouldBe` Nothing

    testCallbacks :: SpecWith repr
    testCallbacks = do
        it "has the correct score" $ \repr -> do
            score :: StandardScore <- runCallback repr
            score `shouldBe` 1002

        it "has the correct score after a bead move" $ \repr -> do
            score :: StandardScore <- updateCallback repr 1002 beadMove
            score `shouldBe` 2002

        it "has the correct score after a binder move" $ \repr -> do
            score :: StandardScore <- updateCallback repr 1002 binderMove
            score `shouldBe` 1000

        it "has the correct gyration radii" $ \repr -> do
            GyrationRadius [c1, c2, c3] <- runCallback repr
            c1 `shouldAlmostBe` 5.92809748
            c2 `shouldAlmostBe` 7.07106781
            c3 `shouldAlmostBe` 1.41421356

        it "has the same gyration radii afer a binder move" $ \repr -> do
            oldRadii :: GyrationRadius <- runCallback repr
            newRadii :: GyrationRadius <- updateCallback repr oldRadii $ Move (V3 0 1 2) (V3 1 0 0)
            oldRadii `shouldBe` newRadii

        it "has the correct gyradion radii afer a bead move" $ \repr -> do
            oldRadii :: GyrationRadius <- runCallback repr
            GyrationRadius [c1, c2, c3] <- updateCallback repr oldRadii $ Move (V3 0 1 1) (V3 0 0 (-1))
            c1 `shouldAlmostBe` 6.34952763
            c2 `shouldAlmostBe` 7.07106781
            c3 `shouldAlmostBe` 1.41421356

        context "when computing the template haskell callbacks" $ do
            it "has the correct sum42-beads" $ \repr -> do
                res :: THCallback "sum42-beads" <- runCallback repr
                res `shouldBe` THCallback (42 * beads)

            it "has the correct prod2-all" $ \repr -> do
                res :: THCallback "prod2-all" <- runCallback repr
                res `shouldBe` THCallback (2 ^ (beads + binders))

            it "has the correct list42-binders" $ \repr -> do
                res :: THCallback "list42-binders" <- runCallback repr
                res `shouldBe` THCallback (replicate binders 42)

            it "has the correct prod-binders-beads" $ \repr -> do
                res :: THCallback "prod-binders-beads" <- runCallback repr
                res `shouldBe` THCallback (binders * beads)

            it "has the correct list-11" $ \repr -> do
                res :: THCallback "list-11" <- runCallback repr
                res `shouldBe` THCallback [sqrt 2, 1]

            it "has the correct sum-11" $ \repr -> do
                res :: THCallback "sum-11" <- runCallback repr
                res `shouldBe` THCallback (1 + sqrt 2)

            it "has the correct pairs-dist<2" $ \repr -> do
                res :: THCallback "pairs-dist<2" <- runCallback repr
                res `shouldBe` THCallback 22

            it "has the correct complex-function" $ \repr -> do
                res :: THCallback "complex-function" <- runCallback repr
                res `shouldBe` complexFunctionResult

            it "has the correct count-lamins" $ \repr -> do
                res :: THCallback "count-lamins" <- runCallback repr
                res `shouldBe` THCallback 2

            it "has the correct score" $ \repr -> do
                res :: THCallback "score" <- runCallback repr
                res `shouldBe` THCallback 1002

    testAfterBeadMove :: SpecWith repr
    testAfterBeadMove = do
        it "reports the old location to be empty" $ \repr -> do
            matom <- getAtomAt (V3 5 6 6) repr
            matom `shouldBe` Nothing

        it "reports the new location to contain the bead" $ \repr -> do
            matom <- getAtomAt (V3 5 6 5) repr
            matom `shouldBe` Just (asAtom $ BeadInfo (V3 5 6 5) ev1 1 0 1)

        it "reports the updated chain" $ \repr -> do
            chain <- getChain repr 0 $ pure . otoList
            chain `shouldBe` updatedChain

        it "reports the binders to be unchanged" $ \repr -> do
            binders <- getBinders repr $ pure . otoList
            binders `shouldMatchList` dumpBinders dump

        context "when dumping" $ beforeWith makeDump $ do
            it "reports the updated chain" $ \dump' ->
                dumpIndexedChains dump' `shouldBe` updatedChains

            it "reports the binders to be unchanged" $ \dump' ->
                dumpBinders dump' `shouldMatchList` dumpBinders dump

        context "when updating callbacks" $ do
            it "has the correct sum-11" $ \repr -> do
                res :: THCallback "sum-11" <- updateCallback repr (THCallback (1 + sqrt 2)) beadMove
                corrRes <- runCallback repr
                res `shouldAlmostBe` corrRes

            it "has the correct pairs-dist<2" $ \repr -> do
                res :: THCallback "pairs-dist<2" <- updateCallback repr (THCallback 22) beadMove
                corrRes <- runCallback repr
                res `shouldBe` corrRes

            it "has the correct complex-function" $ \repr -> do
                res <- updateCallback repr complexFunctionResult beadMove
                corrRes <- runCallback repr
                res `shouldAlmostBe` corrRes

            it "has the correct count-lamins" $ \repr -> do
                res :: THCallback "count-lamins" <- updateCallback repr (THCallback 2) beadMove
                res `shouldBe` THCallback 2

            it "has the correct score" $ \repr -> do
                res :: THCallback "score" <- updateCallback repr (THCallback 1002) beadMove
                res `shouldBe` THCallback 2002

        context "when generating a move"
            testGenerateMove

    testAfterBinderMove :: SpecWith repr
    testAfterBinderMove = do
        it "reports the old location to be empty" $ \repr -> do
            matom <- getAtomAt (V3 0 1 2) repr
            matom `shouldBe` Nothing

        it "reports the new location to contain the binder" $ \repr -> do
            matom <- getAtomAt (V3 1 1 2) repr
            matom `shouldBe` Just (asAtom $ BinderInfo (V3 1 1 2) bi0)

        it "reports the updated binders" $ \repr -> do
            binders <- getBinders repr $ pure . otoList
            binders `shouldMatchList` updatedBinders

        context "when dumping" $ beforeWith makeDump $ do
            it "reports the beads to be unchanged" $ \dump' ->
                dumpIndexedChains dump' `shouldBe` updatedChains

            it "reports the updated binders" $ \dump' ->
                dumpBinders dump' `shouldMatchList` updatedBinders

        context "when generating a move"
            testGenerateMove

    testGenerateMove :: SpecWith repr
    testGenerateMove = modifyMaxSuccess (const 1000) $ do
        it "moves an existing atoms" $ \repr -> monadicIO $ do
            MoveFromTo from _ <- genMove repr
            atom <- getAtomAt from repr
            assert $ isJust atom

        it "moves an atom into an unoccupied position" $ \repr -> monadicIO $ do
            MoveFromTo _ to <- genMove repr
            atom <- getAtomAt to repr
            assert $ isNothing atom

        it "performs only moves with the correct length" $ \repr -> monadicIO $ do
            Move _ diff <- genMove repr
            assert $ quadrance diff `elem` ([1, 2] :: [_])

        context "when generating many moves" $
            beforeWith prepareMoves $ do
                it "fails reasonably rarely" $ \(_, moves) ->
                    length moves `shouldSatisfy` (> 100)

                beforeWith getAtoms $ do
                    it "moves binders sufficiently often" $ \atoms ->
                        length [x | Just (Binder x) <- atoms] `shouldSatisfy` (> 50)

                    it "moves beads sufficiently often" $ \atoms ->
                        length [x | Just (Bead x) <- atoms] `shouldSatisfy` (> 50)

        it "does not move any lamins or frozen beads" $ \repr -> monadicIO $ do
            MoveFromTo from _ <- genMove repr
            Just atom <- getAtomAt from repr
            case atom ^. located of
                BinderSig binder -> assert $ binder ^. binderType /= laminType
                BeadSig bead -> assert . not . freezePredicate $ bead ^. beadSignature
      where
        prepareMoves repr = (repr,) . catMaybes <$>
            (replicateM 1000 . runMaybeT $ generateMove repr)
        getAtoms (repr, moves) = forM moves $ flip getAtomAt repr . moveFrom
        genMove repr = runMaybeT (generateMove repr) >>= maybe (stop rejected) pure

spec :: Spec
spec = do
    context "the pure chain representation" $
        testRepr (Proxy :: Proxy PureChainRepresentation)

    context "the IO chain representation" $
        testRepr (Proxy :: Proxy IOChainRepresentation)

    context "the intersectsChain function"
        testIntersectsChain
