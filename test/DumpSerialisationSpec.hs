{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
module DumpSerialisationSpec where

import Test.Hspec
import Linear
import Data.Maybe
import Data.Foldable

import Bio.Motions.Format.DumpDeserialisation
import Bio.Motions.Format.DumpSerialisation
import Bio.Motions.Format.Proto.Delta

import Bio.Motions.Representation.Dump
import Bio.Motions.Types

import Bio.Motions.Callback.GyrationRadius
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Serialisation
import Bio.Motions.Callback.Periodic

import SimpleCallback

dump :: Dump
dump = Dump
    { dumpBinders =
          [   BinderInfo (V3 0 1 2) bi0
          ,   BinderInfo (V3 0 1 3) bi0
          ,   BinderInfo (V3 5 5 5) bi1
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
  where
    [bi0, bi1] = map BinderType [0, 1]
    (ev0, ev1) = ([1, 0], [0, 1000])

move :: Move
move = Move
    { moveFrom = V3 1 2 3
    , moveDiff = V3 1 1 0
    }

spec :: Spec
spec = context "when serialising and deserialising dumps and moves" $ do
    let h = getHeader "a" "b" ["x", "y", "z"] dump
        kf = getKeyframe dump ([], []) 0
    let Just dumpAgain = deserialiseDump h kf

    it "should return the same dump" $
        dump `shouldBe` dumpAgain
    let delta = serialiseMove move ([], []) 0
    let Just moveAgain = deserialiseMove delta
    it "should return the same move" $
        move `shouldBe` moveAgain
    let serialisedCallbacks =
          toList $ callbacks $ serialiseMove move
              ([CallbackResult $ GyrationRadius [10.1, 2.2]
               , CallbackResult (PeriodicWait 1 :: (Periodic (SimpleCallback 42)))
               , CallbackResult $ PeriodicValue (SimpleCallback 10 :: (SimpleCallback 41))]
              ,[]) 0
    let serialisedCallbacksOK = map fromJust [serialiseCallback "SimpleCallback" (10::Int),
                                              serialiseCallback "Gyration Radius" ([10.1, 2.2]::[Double])]
    it "should return serialised callbacks" $
        serialisedCallbacks `shouldMatchList` serialisedCallbacksOK
