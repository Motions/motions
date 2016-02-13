{-# LANGUAGE OverloadedLists #-}
module DumpSpec where

import Test.Hspec

import Bio.Motions.Representation.Dump
import Bio.Motions.Types
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

spec :: Spec
spec = context "the Dump module" testDump
