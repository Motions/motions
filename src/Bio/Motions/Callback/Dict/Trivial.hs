{- |
Module      : Bio.Motions.Callback.Dict.Trivial
Description : Contains a trivial Callback dictionary
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.Callback.Dict.Trivial(TrivialDict(..)) where

import Bio.Motions.Callback.Class
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Dict

data TrivialDict score = TrivialDict

instance (Monad m, CallbackRepresentation m repr, Score score) => CallbackCache m repr score (TrivialDict score) where
    getPostCallback = runCallback
    {-# INLINE getPostCallback #-}

    getScore = error "getScore called on a TrivialDict"
    {-# INLINE getScore #-}

instance (Monad m, CallbackRepresentation m repr, Score score) => CallbackDict m repr score (TrivialDict score) where
    dictPerformMove _ _ _ = pure ()
    {-# INLINE dictPerformMove #-}
