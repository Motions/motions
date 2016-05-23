{- |
Module      : Bio.Motions.Callback.Dict
Description : Contains the definition of the 'CallbackDict' class
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bio.Motions.Callback.Dict where

import Bio.Motions.Callback.Class
import Bio.Motions.Types

-- |A write-end of a cache.
class CallbackCache m repr score dict => CallbackDict m repr score dict where
    -- |Records a move and updated score.
    dictPerformMove :: dict -> Move -> score -> m ()
