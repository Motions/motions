{- |
Module      : Bio.Motions.Callback.Specialise
Description : Specialised callbacks.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Bio.Motions.Callback.Specialise where

import Bio.Motions.Callback.Class
import Bio.Motions.Representation.Class
import Bio.Motions.Types
import Data.Functor.Identity
import Data.Proxy
import GHC.Exts

data SpecCallback f mode m repr where
    SpecCallback :: Callback mode cb =>
        { scUpdate :: !(repr -> cb -> Move -> m cb)
        , scValue :: !(f cb)
        } -> SpecCallback f mode m repr

type SpecCallbackValue = SpecCallback Identity
type SpecCallbackType = SpecCallback Proxy

pattern SpecCallbackValue upd val = SpecCallback upd (Identity val)

runSpecCallbacks :: forall m mode repr. (ReadRepresentation m repr, Monad m) => repr -> [SpecCallbackType mode m repr] -> m [SpecCallbackValue mode m repr]
runSpecCallbacks repr = mapM go
  where
    go SpecCallback{scUpdate = upd, scValue = _ :: Proxy cb} =
        SpecCallbackValue upd <$> (runCallback repr :: m cb)
{-# INLINE runSpecCallbacks #-}

class SpecCallbacks mode (cbs :: [*]) where
    makeSpecCallbacks :: (ReadRepresentation m repr, Monad m) => proxy cbs -> [SpecCallbackType mode m repr]

instance SpecCallbacks mode '[] where
    makeSpecCallbacks _ = []
    {-# INLINE makeSpecCallbacks #-}

instance (Callback mode cb, SpecCallbacks mode cbs) => SpecCallbacks mode (cb ': cbs) where
    makeSpecCallbacks _ = cur : makeSpecCallbacks (Proxy :: Proxy cbs)
      where cur = SpecCallback (inline updateCallback) (Proxy :: Proxy cb)
    {-# INLINE makeSpecCallbacks #-}

updateSpecCallback :: Functor m => repr -> Move -> SpecCallbackValue mode m repr -> m (SpecCallbackValue mode m repr)
updateSpecCallback repr move (SpecCallback upd (Identity val)) = SpecCallbackValue upd <$> upd repr val move
{-# INLINE updateSpecCallback #-}
