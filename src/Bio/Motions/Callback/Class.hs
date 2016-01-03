{- |
Module      : Bio.Motions.Callback.Class
Description : Contains the definitions of various 'Callback'-related primitives.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
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

-- |Represents the mode of a callback
data Mode = Pre  -- ^Such a callback will be fired before a move is made
          | Post -- ^Such a callback will be fired after a move is made

-- |Represents a callback
--
-- 'm' denotes a 'Monad' (or 'Applicative') in which the callback
-- is willing to operate.
class Callback m (mode :: Mode) cb | cb -> mode where
    -- |Computes the callback's result from scratch.
    runCallback :: ReadRepresentation m repr 
        => repr 
        -- ^The representation.
        -> m cb
        -- ^The computed value.

    -- |Computes the callback's result after a move.
    updateCallback :: ReadRepresentation m repr
        => repr
        -- ^The representation before/after the move. See 'Mode'.
        -> cb
        -- ^The previous value.
        -> Move
        -- ^A move that is about to be/was made. See 'Mode'.
        -> m cb
        -- ^The new value.

    default updateCallback :: (ReadRepresentation m repr, mode ~ 'Post)
        => repr -> cb -> Move -> m cb
    updateCallback repr _ _ = runCallback repr

-- |A convenient existential wrapper around a 'Callback' running in a 'Monad' 'm'
--
-- The result of the callback is required to be 'Show'able due to the need of
-- serialization. TODO: Create a better serializability constraint.
data CallbackWrapper mode m where
    CallbackWrapper :: (Callback m mode cb, Show cb) => cb -> CallbackWrapper mode m

-- |An alias for a particularily important class of callbacks, viz. score functions.
type Score m cb = (Callback m 'Pre cb, Num cb, Ord cb)
