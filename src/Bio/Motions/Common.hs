{- |
Module      : Bio.Motions.Common
Description : Common utility functions for working with common types.
License     : MIT
Stability   : experimental
Portability : unportable
-}

module Bio.Motions.Common where

import Bio.Motions.Types

import Data.MonoTraversable
import qualified Data.Vector.Unboxed as U

isLamin :: BinderType -> Bool
isLamin = (== 0) . getBinderType

doesNotBind :: EnergyVector -> Bool
doesNotBind = oall (== 0) . getEnergyVector

bindsWithLamins :: EnergyVector -> Bool
bindsWithLamins = (/= 0) . (U.! 0) . getEnergyVector
