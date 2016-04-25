{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module CallbackDiscoverSpec(spec) where

import LoadTestCallbacks

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius
import Bio.Motions.Callback.Parser.TH
import Bio.Motions.Callback.Discover
import Bio.Motions.Callback.Periodic
import Data.Proxy
import GHC.Exts
import Test.Hspec

class CallbackNames (callbacks :: [a]) where
    callbackNames :: Proxy# callbacks -> [String]

instance CallbackNames '[] where
    callbackNames _ = []

instance (Callback m cb, CallbackNames cbs) => CallbackNames (cb ': cbs) where
    callbackNames _ = this : rest
      where
        this = callbackName (Proxy :: Proxy cb)
        rest = callbackNames (proxy# :: Proxy# cbs)

testDiscover :: forall discovered expected.
    (CallbackNames discovered, CallbackNames expected)
     => Proxy# discovered -> Proxy# expected -> Spec
testDiscover discovered expected = it "discovers them correctly" $
    discoveredNames `shouldMatchList` expectedNames
  where
    discoveredNames = callbackNames discovered
    expectedNames = callbackNames expected

type PreCallbacks = $(allCallbacks Pre)
type PostCallbacks = $(allCallbacks Post)

spec :: Spec
spec = context "the callback discovery" $ do
    context "when discovering pre-callbacks" $
        testDiscover (proxy# :: Proxy# PreCallbacks) (proxy# :: Proxy#
            '[ StandardScore
             , GyrationRadius
             , Periodic EmptyCallback
             ])

    context "when discovering post-callbacks" $
        testDiscover (proxy# :: Proxy# PostCallbacks) (proxy# :: Proxy#
            '[ THCallback "sum42-beads"
             , THCallback "prod2-all"
             , THCallback "list42-binders"
             , THCallback "pairs-dist<2"
             , THCallback "complex-function"
             , THCallback "count-lamins"
             , THCallback "list-11"
             , THCallback "prod-binders-beads"
             , THCallback "sum-11"
             , THCallback "score"
             ])
