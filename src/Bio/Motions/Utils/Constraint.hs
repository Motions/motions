{- |
Module      : Bio.Motions.Utils.Constraint
Description : Constraint utilities.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Bio.Motions.Utils.Constraint where

import GHC.Exts

-- |Ensures that the 'c' is satisfied for every element of 'list'..
type family All (c :: k -> Constraint) (list :: [k]) :: Constraint where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)
