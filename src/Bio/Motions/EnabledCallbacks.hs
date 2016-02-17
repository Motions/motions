
{- |
Module      : Bio.Motions.EnabledCallbacks
Description : Contains lists of enabled 'Pre' and 'Post' 'Callback's.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
module Bio.Motions.EnabledCallbacks where

import Bio.Motions.Callback.Class

import Data.Proxy

newtype TestCb = TestCb { getTestCb :: Int } deriving Show

instance Callback Pre TestCb where
    callbackName _ = "Test callback"
    runCallback _ = pure . TestCb $ 0
    updateCallback _ old _ = pure . TestCb $ getTestCb old + 1

enabledPreCallbacks :: [CallbackType Pre]
enabledPreCallbacks = [CallbackType (Proxy :: Proxy TestCb)]

enabledPostCallbacks :: [CallbackType Post]
enabledPostCallbacks = []
