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

import Test.Hspec
import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.PureChainRepresentation
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.Parser.TH
import qualified Bio.Motions.Representation.Dump as D

import Control.Monad
import Data.MonoTraversable
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import Linear

import GHC.TypeLits



[callback|CALLBACK "sum42-beads"
    EVERY 1
    NODES 1
    WHERE BELONGS(X 0, BEAD 0) OR BELONGS(X 0, BEAD 1)
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
          AND (BELONGS(X 1, BEAD 0) OR BELONGS(X 1, BEAD 1))
    COMPUTE SUM 1
|]

[callback|CALLBACK "list-11"
    EVERY 1
    NODES 2
    WHERE BELONGS(X 0, BINDER 1) AND BELONGS(X 1, BEAD 1)
    COMPUTE LIST DIST(X 0, X 1)
|]

testRepr :: SpecWith ()
testRepr = do
    repr :: PureChainRepresentation <- loadDump dump

    context "when redumping" $ do
        dump' <- makeDump repr
        it "should yield the same radius" $
            D.radius dump' `shouldBe` D.radius dump
        it "should yield the same bead kinds" $
            D.beadKinds dump' `shouldBe` D.beadKinds dump
        it "should yield the same chains" $
            D.chains dump' `shouldBe` D.chains dump
        it "should yield the same binders" $
            D.binders dump' `shouldMatchList` D.binders dump

    context "when inspecting the data" $ do
        it "should yield the same number of chains" $
            getNumberOfChains repr >>= flip shouldBe (length $ D.chains dump)
        it "should yield the same binders" $ do
            binders <- getBinders repr (pure . otoList)
            binders `shouldBe` D.binders dump
        it "should yield the same beads" $ do
            beads <- forM [0..length (D.chains dump) - 1] $
                \idx -> getChain repr idx (pure . otoList)
            beads `shouldBe` D.chains dump

        context "when using getAtomAt" $ do
            it "should return binders" $
                forM_ (D.binders dump) $ \binder -> do
                    atom <- getAtomAt (binderPosition binder) repr
                    atom `shouldBe` Just (Binder binder)
            it "should return beads" $
                forM_ (concat $ D.chains dump) $ \bead -> do
                    atom <- getAtomAt (beadPosition bead) repr
                    atom `shouldBe` Just (Bead bead)
            it "should return Nothing" $ do
                atom <- getAtomAt (V3 0 0 0) repr
                atom `shouldBe` Nothing

    context "when computing callbacks" $ do
        it "should have the correct score" $ do
            score :: StandardScore <- runCallback repr
            score `shouldBe` 1002

        it "should have the correct score after a bead move" $ do
            score :: StandardScore <- updateCallback repr 1002 $ Move (V3 5 6 6) (V3 0 0 (-1))
            score `shouldBe` 2002

        it "should have the correct score after a binder move" $ do
            score :: StandardScore <- updateCallback repr 1002 $ Move (V3 0 1 2) (V3 1 0 0)
            score `shouldBe` 1000

        context "template haskell callbacks" $ do
            it "should have the correct sum42-beads" $ do
                res :: THCallback "sum42-beads" <- runCallback repr
                res `shouldBe` THCallback (42 * beads)

            it "should have the correct prod2-all" $ do
                res :: THCallback "prod2-all" <- runCallback repr
                res `shouldBe` THCallback (2 ^ (beads + binders))

            it "should have the correct list42-binders" $ do
                res :: THCallback "list42-binders" <- runCallback repr
                res `shouldBe` THCallback (replicate binders 42)

            it "should have the correct prod-binders-beads" $ do
                res :: THCallback "prod-binders-beads" <- runCallback repr
                res `shouldBe` THCallback (binders * beads)

            it "should have the correct list-11" $ do
                res :: THCallback "list-11" <- runCallback repr
                res `shouldBe` THCallback [sqrt 2, 1]


    repr' <- fst <$> performMove (Move (V3 5 6 6) (V3 0 0 (-1))) repr
    dump' <- makeDump repr'

    context "when performing a bead move" $ do
        it "should report the old location to be empty" $ do
            matom <- getAtomAt (V3 5 6 6) repr'
            matom `shouldBe` Nothing
        it "should report the new location to contain the bead" $ do
            matom <- getAtomAt (V3 5 6 5) repr'
            matom `shouldBe` Just (Bead $ BeadInfo (V3 5 6 5) be1 ev1 1 0 1)
        it "should report the updated chain" $ do
            chain <- getChain repr' 0 $ pure . otoList
            chain `shouldBe` [ BeadInfo (V3 0 1 1) be0 ev0 0 0 0
                             , BeadInfo (V3 5 6 5) be1 ev1 1 0 1
                             , BeadInfo (V3 5 5 6) be0 ev0 2 0 2
                             ]
            chain `shouldBe` head (D.chains dump')
        it "should report the binders to be unchanged" $ do
            binders <- getBinders repr' $ pure . otoList
            binders `shouldMatchList` D.binders dump
            binders `shouldMatchList` D.binders dump'

    repr'' <- fst <$> performMove (Move (V3 0 1 2) (V3 1 0 0)) repr'
    dump'' <- makeDump repr''

    context "when performing a binder move" $ do
        it "should report the old location to be empty" $ do
            matom <- getAtomAt (V3 0 1 2) repr''
            matom `shouldBe` Nothing
        it "should report the new location to contain the binder" $ do
            matom <- getAtomAt (V3 1 1 2) repr''
            matom `shouldBe` Just (Binder $ BinderInfo (V3 1 1 2) bi0)
        it "should not report the updated binders" $ do
            binders <- getBinders repr'' $ pure . otoList
            binders `shouldMatchList` [ BinderInfo (V3 1 1 2) bi0
                                      , BinderInfo (V3 0 1 3) bi0
                                      , BinderInfo (V3 5 5 5) bi1
                                      ]
            binders `shouldMatchList` D.binders dump''
        it "should report the beads to be unchanged" $ do
            D.chains dump'' `shouldBe` D.chains dump'

  where
    beads = sum $ map length $ D.chains dump
    binders = length $ D.binders dump
    dump = D.Dump
        { D.radius = 10
        , D.binders =
            [ BinderInfo (V3 0 1 2) bi0
            , BinderInfo (V3 0 1 3) bi0
            , BinderInfo (V3 5 5 5) bi1
            ]
        , D.chains =
            [ [ BeadInfo (V3 0 1 1) be0 ev0 0 0 0
              , BeadInfo (V3 5 6 6) be1 ev1 1 0 1
              , BeadInfo (V3 5 5 6) be0 ev0 2 0 2
              ]
            , [ BeadInfo (V3 0 0 2) be0 ev0 3 1 0
              , BeadInfo (V3 5 4 5) be1 ev1 4 1 1
              ]
            ]
        , D.beadKinds = [ev0, ev1]
        }
    [bi0, bi1] = BinderType <$> [0, 1]
    [be0, be1] = BeadType <$> [0, 1]
    [ev0, ev1] = EnergyVector . U.fromList <$> [[1, 0], [0, 1000]]

main :: IO ()
main = hspec $ context "the pure chain representation" testRepr
