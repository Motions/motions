{- |
Module      : Bio.Motions.Callback.Periodic
Description : Contains the Periodic callback transformer
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Bio.Motions.Callback.Periodic
    ( CallbackPeriod
    , Periodic(..)
    ) where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Serialisation
import Data.Proxy
import GHC.TypeLits
import GHC.Generics
import Control.DeepSeq

-- |The interval between successive recomputations of a 'Callback' 'cb', when transformed
-- using the 'Periodic' callback transformer
type family CallbackPeriod cb :: Nat

-- |Transforms a 'Callback' 'cb' into a 'Callback' which is run every 'CallbackPeriod cb' steps.
data Periodic cb = PeriodicValue { periodicValue :: !cb }
                 -- ^The value of 'cb' is present right now
                 | PeriodicWait { periodicWait :: !Int }
                 -- ^The value will be computed in 'periodicWait' steps.
                 deriving (Eq, Generic, NFData)

instance Show cb => Show (Periodic cb) where
    show (PeriodicValue v) = show v
    show _ = "---"

instance CallbackSerialisable cb => CallbackSerialisable (Periodic cb) where
    serialiseCallback name (PeriodicValue v) = serialiseCallback name v
    serialiseCallback _ _ = Nothing

-- | Note: The leading '_' is dropped from the base callback's name, if present.
instance (Callback mode cb, KnownNat (CallbackPeriod cb), CmpNat 0 (CallbackPeriod cb) ~ 'LT)
          => Callback mode (Periodic cb) where
    callbackName _ = case callbackName (Proxy :: Proxy cb) of
        '_' : name -> name
        name -> name
    {-# INLINEABLE callbackName #-}

    runCallback cache = fmap PeriodicValue . runCallback cache
    {-# INLINE runCallback #-}

    -- |Updates the value of the callback only once in 'CallbackPeriod cb' steps.
    updateCallback cache repr (PeriodicWait 1) _ = runCallback cache repr
    updateCallback _ _ (PeriodicWait n) _ = pure . PeriodicWait $ n - 1
    updateCallback cache repr (PeriodicValue cb) move
        | period == 1 = PeriodicValue <$> updateCallback cache repr cb move
        | otherwise = pure $ PeriodicWait period
        where period = fromInteger $ natVal (Proxy :: Proxy (CallbackPeriod cb))
    {-# INLINEABLE updateCallback #-}
