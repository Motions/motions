{-# LANGUAGE OverloadedLists #-}
module DumpSpec where

import Test.Hspec

import Bio.Motions.Representation.Dump
import Bio.Motions.Types
import Bio.Motions.Common
import Data.Either
import Linear

testDump :: Spec
testDump =
    it "adds indices correctly" $
        addIndices dbiChains `shouldBe` biChains
  where
    dbiChains =
        [ [ DumpBeadInfo (V3 0 0 0) ev0
          , DumpBeadInfo (V3 0 1 0) ev1
          , DumpBeadInfo (V3 1 1 0) ev0
          ]
        , [ DumpBeadInfo (V3 4 2 1) ev0
          ]
        , [ DumpBeadInfo (V3 3 2 1) ev1
          , DumpBeadInfo (V3 3 2 2) ev0
          ]
        ]
    biChains =
        [ [ BeadInfo (V3 0 0 0) ev0 0 0 0
          , BeadInfo (V3 0 1 0) ev1 1 0 1
          , BeadInfo (V3 1 1 0) ev0 2 0 2
          ]
        , [ BeadInfo (V3 4 2 1) ev0 3 1 0
          ]
        , [ BeadInfo (V3 3 2 1) ev1 4 2 0
          , BeadInfo (V3 3 2 2) ev0 5 2 1
          ]
        ]
    (ev0, ev1) = ([1, 2], [0, 2])

testDumpDiff :: Spec
testDumpDiff =
    context "when computing Dump difference" $ do
        it "computes a correct binder difference" $
            diffDumps dump1 dump2 `shouldBe` Right (MoveFromTo (V3 1 1 1) (V3 1 1 2))
        it "computes a correct bead difference" $
            diffDumps dump1 dump2' `shouldBe` Right (MoveFromTo (V3 1 0 0) (V3 1 1 0))
        it "fails when no difference was found" $
            diffDumps dump1 dump1 `shouldSatisfy` isLeft
        it "fails when a lamin binder has moved" $
            diffDumps dump1 dump1MovedLamin `shouldSatisfy` isLeft
        it "fails when chain counts don't match" $
            diffDumps dump1 dump2ExtraChain `shouldSatisfy` isLeft
        it "fails when corresponding chains have different length" $
            diffDumps dump1 dump2ExtraBead `shouldSatisfy` isLeft
        it "fails when corresponding beads have different EVs" $
            diffDumps dump1 dump2ModifiedEV `shouldSatisfy` isLeft
        it "fails when corresponding binders have different types" $
            diffDumps dump1 dump2ModifiedBT `shouldSatisfy` isLeft
        it "fails when two differences were found" $
            diffDumps dump1 dump2ExtraDiff `shouldSatisfy` isLeft
  where
    dump1 :: Dump
    dump1 = Dump dump1Binders dump1Chains

    dump2 :: Dump
    dump2 = Dump dump2Binders dump1Chains

    dump2' :: Dump
    dump2' = Dump dump1Binders dump2'Chains

    dump1MovedLamin :: Dump
    dump1MovedLamin = Dump dump1BindersMovedLamin dump1Chains

    dump2ExtraChain :: Dump
    dump2ExtraChain = Dump dump2Binders dump1ChainsExtraChain

    dump2ExtraBead :: Dump
    dump2ExtraBead = Dump dump2Binders dump1ChainsExtraBead

    dump2ModifiedEV :: Dump
    dump2ModifiedEV = Dump dump2Binders dump1ChainsModifiedEV

    dump2ModifiedBT :: Dump
    dump2ModifiedBT = Dump dump2BindersModifiedBT dump1Chains

    dump2ExtraDiff :: Dump
    dump2ExtraDiff = Dump  dump2Binders dump2'Chains

    dump1Binders :: [BinderInfo]
    dump1Binders =
        [ BinderInfo (V3 0 1 0) bi0
        , BinderInfo (V3 1 1 1) bi1
        ]

    dump2Binders :: [BinderInfo]
    dump2Binders =
        [ BinderInfo (V3 0 1 0) bi0
        , BinderInfo (V3 1 1 2) bi1
        ]

    dump1Chains :: [[DumpBeadInfo]]
    dump1Chains =
        [ [ DumpBeadInfo (V3 0 0 0) ev0
          , DumpBeadInfo (V3 0 0 1) ev1
          ]
        , [ DumpBeadInfo (V3 1 0 0) ev0
          , DumpBeadInfo (V3 2 0 0) ev0
          , DumpBeadInfo (V3 3 1 0) ev0
          ]
        ]

    dump2'Chains :: [[DumpBeadInfo]]
    dump2'Chains =
        [ [ DumpBeadInfo (V3 0 0 0) ev0
          , DumpBeadInfo (V3 0 0 1) ev1
          ]
        , [ DumpBeadInfo (V3 1 1 0) ev0
          , DumpBeadInfo (V3 2 0 0) ev0
          , DumpBeadInfo (V3 3 1 0) ev0
          ]
        ]

    dump1BindersMovedLamin :: [BinderInfo]
    dump1BindersMovedLamin =
        [ BinderInfo (V3 0 2 0) bi0
        , BinderInfo (V3 1 1 1) bi1
        ]

    dump1ChainsExtraChain :: [[DumpBeadInfo]]
    dump1ChainsExtraChain = dump1Chains ++ [[ DumpBeadInfo (V3 4 0 0) ev0 ]]

    dump1ChainsExtraBead :: [[DumpBeadInfo]]
    dump1ChainsExtraBead =
        [ [ DumpBeadInfo (V3 0 0 0) ev0
          , DumpBeadInfo (V3 0 0 1) ev1
          ]
        , [ DumpBeadInfo (V3 1 0 0) ev0
          , DumpBeadInfo (V3 2 0 0) ev0
          , DumpBeadInfo (V3 3 1 0) ev0
          , DumpBeadInfo (V3 4 0 0) ev0
          ]
        ]

    dump1ChainsModifiedEV :: [[DumpBeadInfo]]
    dump1ChainsModifiedEV =
        [ [ DumpBeadInfo (V3 0 0 0) ev0
          , DumpBeadInfo (V3 0 0 1) ev1
          ]
        , [ DumpBeadInfo (V3 1 0 0) ev0
          , DumpBeadInfo (V3 2 0 0) ev1
          , DumpBeadInfo (V3 3 1 0) ev0
          ]
        ]

    dump2BindersModifiedBT :: [BinderInfo]
    dump2BindersModifiedBT =
        [ BinderInfo (V3 0 1 0) bi1
        , BinderInfo (V3 1 1 2) bi1
        ]

    (bi0, bi1) = (laminType, BinderType 1)
    (ev0, ev1) = ([0,0], [0,1])

spec :: Spec
spec = context "the Dump module" $ testDump >> testDumpDiff
