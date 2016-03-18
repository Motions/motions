{- |
Module      : Bio.Motions.Callback.Discover
Description : Contains the automatic callback discovery module.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Motions.Callback.Discover where

import Bio.Motions.Callback.Class
import Data.Proxy
import Language.Haskell.TH

-- |Finds all callbacks with the given 'Mode' and creates a list of
-- their 'CallbackType's.
--
-- Note: this function should be run (in a splice) when instances of
-- all defined callbacks are in scope. In particular, when a callback
-- is parsed using the Template Haskell callback quasiquoter, it must
-- be parsed at stage prior to the call to 'allCallbacks', e.g. it must
-- be defined in another module.
allCallbacks :: Mode -> ExpQ
allCallbacks mode = do
    name <- newName "a"
    mode' <- promotedT $ case mode of
        Pre -> 'Pre
        Post -> 'Post
    insts <- reifyInstances ''Callback [mode', VarT name]
    listE [mkCallbackType t | InstanceD _ (AppT _ t) _ <- insts]
  where
    mkCallbackType t = [| CallbackType (Proxy :: Proxy $(pure t)) |]
