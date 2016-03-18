{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CallbackPeriodicSpec(spec) where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Periodic
import Bio.Motions.Representation.Class
import Data.Proxy
import GHC.TypeLits
import Test.Hspec

newtype SimpleCallback (n :: Nat) = SimpleCallback Int
    deriving (Show, Eq, Num)

instance Callback 'Pre (SimpleCallback n) where
    callbackName _ = "_SimpleCallback"

    runCallback _ = pure $ SimpleCallback 0

    updateCallback _ (SimpleCallback n) _ = pure . SimpleCallback $ n + 1

type instance CallbackPeriod (SimpleCallback n) = n

-- |Make sure that the 'Periodic' callback transformer does not call any representation-related
-- function directly
data EmptyRepresentation
instance ReadRepresentation m EmptyRepresentation where
    getBinders = undefined
    getNumberOfChains = undefined
    getChain = undefined
    getAtomAt = undefined

spec :: Spec
spec = context "periodic callback transformer" $ do
    it "has the correct name" $
        callbackName (Proxy :: Proxy (Periodic (SimpleCallback 42))) `shouldBe` "SimpleCallback"

    it "performs runCallback correctly" $ do
        cb :: Periodic (SimpleCallback 42) <- runCallback repr
        cb `shouldBe` PeriodicValue 0

    context "when performing updateCallback" $ do
        context "on a callback with period 1" $
            it "calls updateCallback on the base callback" $ do
                cb <- updateCallback repr (PeriodicValue $ SimpleCallback 42) undefined
                cb `shouldBe` PeriodicValue (SimpleCallback 43 :: SimpleCallback 1)

        context "on a callback with period >1" $ do
            it "calls runCallback when the waiting period is over" $ do
                cb <- updateCallback repr (PeriodicWait 1) undefined
                cb `shouldBe` PeriodicValue (SimpleCallback 0 :: SimpleCallback 10)

            it "decrements the timer correctly" $ do
                cb <- updateCallback repr (PeriodicWait 3) undefined
                cb `shouldBe` (PeriodicWait 2 :: Periodic (SimpleCallback 10))

            it "enters the waiting period after producing a value" $ do
                cb <- updateCallback repr (PeriodicValue $ SimpleCallback 42) undefined
                cb `shouldBe` (PeriodicWait 10 :: Periodic (SimpleCallback 10))
    where repr = undefined :: EmptyRepresentation
