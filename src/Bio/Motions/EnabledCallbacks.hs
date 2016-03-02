
{- |
Module      : Bio.Motions.EnabledCallbacks
Description : Contains lists of enabled 'Pre' and 'Post' 'Callback's.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Bio.Motions.EnabledCallbacks where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.GyrationRadius
import Bio.Motions.Callback.Parser.TH

import Data.Proxy

[callback|CALLBACK "sum42-beads-lamin"
    EVERY 1
    NODES 1
    WHERE BELONGS(X 0, BEAD_BINDING_TO 0)
    COMPUTE SUM 42
|]

enabledPreCallbacks :: [CallbackType Pre]
enabledPreCallbacks = [CallbackType (Proxy :: Proxy GyrationRadius)]

enabledPostCallbacks :: [CallbackType Post]
enabledPostCallbacks = [CallbackType (Proxy :: Proxy (THCallback "sum42-beads-lamin"))]
