{- |
Module      : Bio.Motions.Callback.Dict.Trivial
Description : Contains a trivial Callback dictionary
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bio.Motions.Callback.Dict.Trivial(TrivialDict(..)) where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Dict

data TrivialDict = TrivialDict

instance (Monad m, ReadRepresentation m repr) => CallbackCache m repr TrivialDict where
    getPostCallback = runCallback
    {-# INLINE getPostCallback #-}

instance (Monad m, ReadRepresentation m repr) => CallbackDict m repr TrivialDict where
    dictPerformMove _ _ = pure TrivialDict
    {-# INLINE dictPerformMove #-}
