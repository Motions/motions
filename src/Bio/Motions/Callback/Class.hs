{- |
Module      : Bio.Motions.Callback.Class
Description : Contains the definitions of various 'Callback'-related primitives.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bio.Motions.Callback.Class where

import Bio.Motions.Types
import Bio.Motions.Representation.Class

import Data.Proxy

-- |Represents the mode of a callback
data Mode = Pre  -- ^Such a callback will be fired before a move is made
          | Post -- ^Such a callback will be fired after a move is made

-- |Represents a callback
--
class Show cb => Callback (mode :: Mode) cb | cb -> mode where
    -- |A human-readable name of the callback.
    callbackName :: Proxy cb -> String

    -- |Computes the callback's result from scratch.
    runCallback :: (Monad m, ReadRepresentation m repr)
        => repr
        -- ^The representation.
        -> m cb
        -- ^The computed value.

    -- |Computes the callback's result after a move.
    updateCallback :: (Monad m, ReadRepresentation m repr)
        => repr
        -- ^The representation before/after the move. See 'Mode'.
        -> cb
        -- ^The previous value.
        -> Move
        -- ^A move that is about to be/was made. See 'Mode'.
        -> m cb
        -- ^The new value.

    default updateCallback :: (Monad m, ReadRepresentation m repr, mode ~ 'Post)
        => repr -> cb -> Move -> m cb
    updateCallback repr _ _ = runCallback repr

-- |A convenient existential wrapper around a 'Callback' running in a 'Monad' 'm'
data CallbackWrapper mode where
    CallbackWrapper :: Callback mode cb => cb -> CallbackWrapper mode

data CallbackType mode where
    CallbackType :: Callback mode cb => Proxy cb -> CallbackType mode

getCallbackName :: forall cb mode. Callback mode cb => cb -> String
getCallbackName (cb :: cb) = callbackName (Proxy :: Proxy cb)

getCallbackResults :: forall t m repr mode. (Traversable t, Monad m, ReadRepresentation m repr) =>
    repr -> t (CallbackType mode) -> m (t (CallbackWrapper mode))
getCallbackResults = traverse . getCallbackResult

getCallbackResult :: forall m repr mode. (Monad m, ReadRepresentation m repr) =>
    repr -> CallbackType mode -> m (CallbackWrapper mode)
getCallbackResult repr (CallbackType (Proxy :: Proxy cb)) = CallbackWrapper <$> (runCallback repr :: m cb)

updateCallbackWrapper :: (Monad m, ReadRepresentation m repr) =>
    repr -> Move -> CallbackWrapper mode -> m (CallbackWrapper mode)
updateCallbackWrapper repr move (CallbackWrapper cb) = CallbackWrapper <$> updateCallback repr cb move

-- |An alias for a particularily important class of callbacks, viz. score functions.
-- TODO: better serializability constraint
type Score cb = (Callback 'Pre cb, Num cb, Ord cb, Integral cb, Show cb)
