
{- |
Module      : Bio.Motions.EnabledCallbacks
Description : Contains lists of enabled 'Pre' and 'Post' 'Callback's.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE DataKinds #-}
module Bio.Motions.EnabledCallbacks where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.GyrationRadius

import Data.Proxy

enabledPreCallbacks :: [CallbackType Pre]
enabledPreCallbacks = [CallbackType (Proxy :: Proxy GyrationRadius)]

enabledPostCallbacks :: [CallbackType Post]
enabledPostCallbacks = []
