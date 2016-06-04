{- |
Module      : Bio.Motions.Callback.Class
Description : Contains the definitions of various 'Callback'-related primitives.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bio.Motions.Callback.Class where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Serialisation

import Data.Typeable
import Control.DeepSeq

type Callbacks = ([CallbackResult 'Pre], [CallbackResult 'Post])

-- |Represents the mode of a callback
data Mode = Pre  -- ^Such a callback will be fired before a move is made
          | Post -- ^Such a callback will be fired after a move is made

-- |Represents a callback
class (Show cb, CallbackSerialisable cb, NFData cb, Typeable cb) => Callback (mode :: Mode) cb | cb -> mode where
    -- |A human-readable name of the callback.
    callbackName :: proxy cb -> String

    -- |Computes the callback's result from scratch.
    runCallback :: CallbackCache m repr score cache
        => cache
        -- ^The callback cache.
        -> repr
        -- ^The representation.
        -> m cb
        -- ^The computed value.

    -- |Computes the callback's result after a move.
    updateCallback :: CallbackCache m repr score cache
        => cache
        -- ^The callback cache.
        -> repr
        -- ^The representation before/after the move. See 'Mode'.
        -> cb
        -- ^The previous value.
        -> Move
        -- ^A move that is about to be/was made. See 'Mode'.
        -> m cb
        -- ^The new value.

    default updateCallback :: (CallbackCache m repr score cache, mode ~ 'Post)
        => cache -> repr -> cb -> Move -> m cb
    updateCallback cache repr _ _ = runCallback cache repr
    {-# INLINEABLE updateCallback #-}

-- |An existential wrapper around a 'Callback''s result.
data CallbackResult mode where
    CallbackResult :: (Callback mode cb) => !cb -> CallbackResult mode

instance NFData (CallbackResult mode) where
    rnf (CallbackResult cb) = rnf cb

-- |An existential wrapper around a 'Callback''s type.
data CallbackType mode where
    CallbackType :: Callback mode cb => Proxy cb -> CallbackType mode

getCallbackName :: forall cb mode. Callback mode cb => cb -> String
getCallbackName _ = callbackName (Proxy :: Proxy cb)

-- |An alias for a particularily important class of callbacks, viz. score functions.
-- TODO: better serializability constraint
-- TODO: remove Integral
type Score cb = (Callback 'Pre cb, Num cb, Ord cb, Integral cb, Show cb)

-- |Represents callback value cache
--
-- For optimisation purposes (i.e. specialisations), monad and representation
-- are provided as type class parameters.
class (Monad m, CallbackRepresentation m repr, Score score) => CallbackCache m repr score cache | cache -> score where
    -- |Computes the value of a callback or returns a cached one
    --
    -- It is allowed to use any optimisations it finds suitable,
    -- provided that the return values are correct
    getPostCallback :: Callback 'Post cb => cache -> repr -> m cb

    -- |Returns the current value of the score function.
    getScore :: cache -> m score
