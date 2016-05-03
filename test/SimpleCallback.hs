{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleCallback where

import GHC.TypeLits

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Serialisation
import Bio.Motions.Callback.Periodic

import Control.DeepSeq

newtype SimpleCallback (n :: Nat) = SimpleCallback Int
    deriving (Show, Eq, Num, CallbackSerialisable, NFData)

instance Callback 'Pre (SimpleCallback n) where
    callbackName _ = "_SimpleCallback"

    runCallback _ = pure $ SimpleCallback 0

    updateCallback _ (SimpleCallback n) _ = pure . SimpleCallback $ n + 1

type instance CallbackPeriod (SimpleCallback n) = n
