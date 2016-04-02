{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module CallbackSpec where


import LoadTestCallbacks()

import Test.Hspec

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Chain.Internal
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius
import Bio.Motions.Callback.Parser.TH
import Bio.Motions.Representation.Dump

import Linear
import Control.Lens
import Data.Proxy

--TODO dup
shouldAlmostBe :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
x `shouldAlmostBe` y = abs (x - y) `shouldSatisfy` (< 1e-7)


testRepr :: _ => proxy repr -> Spec
testRepr (_ :: _ repr) = before (loadDump dump freezePredicate :: IO repr) $ do
    context "when computing callbacks"
        testCallbacks

    beforeWith (\repr -> fst <$> performMove beadMove repr) $
        context "after making a bead move" $ do
            testAfterBeadMove

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

spec :: Spec
spec = do
    context "the pure chain representation" $
        testRepr (Proxy :: Proxy PureChainRepresentation)

    context "the IO chain representation" $
        testRepr (Proxy :: Proxy IOChainRepresentation)

