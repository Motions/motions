{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module CallbackSpec where

import LoadTestCallbacks()

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius
import Bio.Motions.Callback.Parser.TH
import Bio.Motions.Callback.Discover
import Data.Proxy
import GHC.Exts
import Test.Hspec

class CallbackNames (m :: Mode) (callbacks :: [a]) where
    callbackNames :: Proxy# m -> Proxy# callbacks -> [String]

instance CallbackNames m '[] where
    callbackNames _ _ = []

instance (Callback m cb, CallbackNames m cbs) => CallbackNames m (cb ': cbs) where
    callbackNames pM _ = this : rest
      where
        this = callbackName (Proxy :: Proxy cb)
        rest = callbackNames pM (proxy# :: Proxy# cbs)

testDiscover :: forall mode list. CallbackNames mode list => [CallbackType mode] -> Proxy# list -> Spec
testDiscover discovered pList = it "discovers them correctly" $
    discoveredNames `shouldMatchList` expectedNames
  where
    discoveredNames = [callbackName t | CallbackType t <- discovered]
    expectedNames = callbackNames (proxy# :: Proxy# mode) pList

preCallbacks :: [CallbackType 'Pre]
preCallbacks = $(allCallbacks Pre)

postCallbacks :: [CallbackType 'Post]
postCallbacks = $(allCallbacks Post)

spec :: Spec
spec = context "the callback discovery" $ do
    context "when discovering pre-callbacks" $
        testDiscover preCallbacks (proxy# :: Proxy#
            '[ StandardScore
             , GyrationRadius
             ])

    context "when discovering post-callbacks" $
        testDiscover postCallbacks (proxy# :: Proxy#
            '[ THCallback "sum42-beads"
             , THCallback "prod2-all"
             , THCallback "list42-binders"
             , THCallback "pairs-dist<2"
             , THCallback "complex-function"
             , THCallback "count-lamins"
             , THCallback "list-11"
             , THCallback "prod-binders-beads"
             , THCallback "sum-11"
             ])
