{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BasicSpec where

import Test.Hspec
import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Chain.Internal (PureChainRepresentation, intersectsChain, space)
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.Parser.TH
import Bio.Motions.Representation.Dump

import Control.Monad
import Data.MonoTraversable
import Data.Proxy
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Linear

import GHC.TypeLits

[callback|CALLBACK "sum42-beads"
    EVERY 1
    NODES 1
    WHERE BELONGS(X 0, BEAD_BINDING_TO 0) OR BELONGS(X 0, BEAD_BINDING_TO 1)
    COMPUTE SUM 42
|]

[callback|CALLBACK "prod2-all"
    EVERY 1
    NODES 1
    WHERE 1 == 1
    COMPUTE PRODUCT 2
|]

[callback|CALLBACK "list42-binders"
    EVERY 1
    NODES 1
    WHERE BELONGS(X 0, BINDER 0) OR BELONGS(X 0, BINDER 1)
    COMPUTE LIST 42
|]

[callback|CALLBACK "prod-binders-beads"
    EVERY 1
    NODES 2
    WHERE (BELONGS(X 0, BINDER 0) OR BELONGS(X 0, BINDER 1))
          AND (BELONGS(X 1, BEAD_BINDING_TO 0) OR BELONGS(X 1, BEAD_BINDING_TO 1))
    COMPUTE SUM 1
|]

[callback|CALLBACK "list-11"
    EVERY 1
    NODES 2
    WHERE BELONGS(X 0, BINDER 1) AND BELONGS(X 1, BEAD_BINDING_TO 1)
    COMPUTE LIST DIST(X 0, X 1)
|]

testIntersectsChain :: Spec
testIntersectsChain = do
    it "reports actual intersections to exist" $
        intersectsChain space (V3 7 8 7) (V3 7 7 8) `shouldBe` True

    it "doesn't report about non-existing intersections" $
        intersectsChain space (V3 7 8 7) (V3 7 8 8) `shouldBe` False
  where
    space = M.fromList
        [ (V3 7 7 7, Bead $ BeadInfo (V3 7 7 7) ev 0 0 0)
        , (V3 7 8 8, Bead $ BeadInfo (V3 7 8 8) ev 0 0 1)
        ]
    ev = EnergyVector U.empty

testRepr :: forall proxy repr. Representation IO repr => proxy repr -> Spec
testRepr _ = before (loadDump dump :: IO repr) $ do
    context "when redumping" $
        beforeWith makeDump testRedump

    context "when inspecting the data"
        testInspect

    context "when computing callbacks"
        testCallbacks

    beforeWith (\repr -> fst <$> performMove (Move (V3 5 6 6) (V3 0 0 (-1))) repr) $
        context "after making a bead move" $ do
            testAfterBeadMove

            beforeWith (\repr -> fst <$> performMove (Move (V3 0 1 2) (V3 1 0 0)) repr) $
                context "after making a binder move"
                    testAfterBinderMove
  where
    beads = sum . map length $ dumpIndexedChains dump
    binders = length $ dumpBinders dump
    dump = Dump
        { dumpRadius = 10
        , dumpBinders =
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
    [bi0, bi1] = BinderType <$> [0, 1]
    [ev0, ev1] = EnergyVector . U.fromList <$> [[1, 0], [0, 1000]]

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
        it "yields the same radius" $ \dump' ->
            dumpRadius dump' `shouldBe` dumpRadius dump

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
                    atom <- getAtomAt (binderPosition binder) repr
                    atom `shouldBe` Just (Binder binder)

            it "returns beads" $ \repr ->
                forM_ (concat $ dumpIndexedChains dump) $ \bead -> do
                    atom <- getAtomAt (beadPosition bead) repr
                    atom `shouldBe` Just (Bead bead)

            it "returns Nothing" $ \repr -> do
                atom <- getAtomAt (V3 0 0 0) repr
                atom `shouldBe` Nothing

    testCallbacks :: SpecWith repr
    testCallbacks = do
        it "has the correct score" $ \repr -> do
            score :: StandardScore <- runCallback repr
            score `shouldBe` 1002

        it "has the correct score after a bead move" $ \repr -> do
            score :: StandardScore <- updateCallback repr 1002 $ Move (V3 5 6 6) (V3 0 0 (-1))
            score `shouldBe` 2002

        it "has the correct score after a binder move" $ \repr -> do
            score :: StandardScore <- updateCallback repr 1002 $ Move (V3 0 1 2) (V3 1 0 0)
            score `shouldBe` 1000

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

    testAfterBeadMove :: SpecWith repr
    testAfterBeadMove = do
        it "reports the old location to be empty" $ \repr -> do
            matom <- getAtomAt (V3 5 6 6) repr
            matom `shouldBe` Nothing

        it "reports the new location to contain the bead" $ \repr -> do
            matom <- getAtomAt (V3 5 6 5) repr
            matom `shouldBe` Just (Bead $ BeadInfo (V3 5 6 5) ev1 1 0 1)

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

    testAfterBinderMove :: SpecWith repr
    testAfterBinderMove = do
        it "reports the old location to be empty" $ \repr -> do
            matom <- getAtomAt (V3 0 1 2) repr
            matom `shouldBe` Nothing

        it "reports the new location to contain the binder" $ \repr -> do
            matom <- getAtomAt (V3 1 1 2) repr
            matom `shouldBe` Just (Binder $ BinderInfo (V3 1 1 2) bi0)

        it "reports the updated binders" $ \repr -> do
            binders <- getBinders repr $ pure . otoList
            binders `shouldMatchList` updatedBinders

        context "when dumping" $ beforeWith makeDump $ do
            it "reports the beads to be unchanged" $ \dump' ->
                dumpIndexedChains dump' `shouldBe` updatedChains

            it "reports the updated binders" $ \dump' ->
                dumpBinders dump' `shouldMatchList` updatedBinders

spec :: Spec
spec = do
    context "the pure chain representation" $
        testRepr (Proxy :: Proxy PureChainRepresentation)

    context "the intersectsChain function"
        testIntersectsChain
