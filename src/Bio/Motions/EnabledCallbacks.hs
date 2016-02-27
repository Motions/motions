
{- |
Module      : Bio.Motions.EnabledCallbacks
Description : Contains lists of enabled 'Pre' and 'Post' 'Callback's.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bio.Motions.EnabledCallbacks where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.GyrationRadius

import Data.Proxy

newtype TestCb = TestCb { getTestCb :: Int }
    deriving (Eq, Ord, Num, Show, Integral, Enum, Real)

instance Callback Pre TestCb where
    callbackName _ = "Test callback"
    runCallback _ = pure 0
    updateCallback _ old _ = pure $ old + 1

enabledPreCallbacks :: [CallbackType Pre]
enabledPreCallbacks = [CallbackType (Proxy :: Proxy TestCb)]

enabledPostCallbacks :: [CallbackType Post]
enabledPostCallbacks = []
