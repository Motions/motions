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

class CallbackCache m repr dict => CallbackDict m repr dict where
    dictPerformMove :: dict -> Move -> m ()
