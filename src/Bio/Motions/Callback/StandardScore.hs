{- |
Module      : Bio.Motions.Callback.StandardScore
Description : Contains the definition of the standard score function.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Bio.Motions.Callback.StandardScore(StandardScore) where

import Bio.Motions.Types
import Bio.Motions.Callback.Class
import Bio.Motions.Representation.Class
import Control.Lens
import Data.List
import Data.MonoTraversable
import Data.Foldable
import Linear

{- |
Represents the standard score function, i.e. the sum over all contacts of the binding energy
between the contacting atoms. Contacts are defined as pairs (binder, bead) with unit l_1
distance.
 -}
newtype StandardScore = StandardScore Int
    deriving (Eq, Ord, Num, Show, Integral, Enum, Real)

instance Monoid StandardScore where
    mempty = 0
    {-# INLINE mempty #-}

    mappend = (+)
    {-# INLINE mappend #-}

instance Monad m => Callback m 'Pre StandardScore where
    runCallback repr = do
        numChains <- getNumberOfChains repr
        fold <$> traverse (chainScore repr) [0..numChains-1]

    updateCallback repr prev (MoveFromTo moveFrom moveTo) = do
        Just fromAtom <- getAtomAt moveFrom repr
        atFrom <- energyToMany repr fromAtom $ neighbours fromAtom
        atTo <- energyToMany repr fromAtom $ delete moveFrom $ neighbours moveTo
        pure $ prev - atFrom + atTo

-- |Returns the score between an object and the atom placed on the specified position.
energyTo :: (Functor m, ReadRepresentation m repr, HasPosition obj, HaveEnergyBetween obj (Maybe Atom)) =>
    repr -> obj -> Vec3 -> m StandardScore
energyTo repr obj pos = StandardScore . energyBetween obj <$> getAtomAt pos repr

-- |Returns the total score between an object (e.g. an atom) and the atoms placed on the
-- specified positions.
energyToMany :: (Applicative m, ReadRepresentation m repr, HasPosition obj,
    HaveEnergyBetween obj (Maybe Atom), Traversable t) =>
    repr -> obj -> t Vec3 -> m StandardScore
energyToMany repr obj poss = fold <$> traverse (energyTo repr obj) poss

-- |Returns the neighbour positions for a position or something with position (e.g. an atom).
neighbours :: HasPosition x => x -> [Vec3]
neighbours x = ((x ^. position) ^+^) <$> ([id, negated] <*> basis)

-- |Returns the total score for beads belonging to a particular chain.
chainScore :: (Monad m, ReadRepresentation m repr) => repr -> Int -> m StandardScore
chainScore repr idx = getChain repr idx $ ofoldlM combine mempty
  where
    combine acc beadInfo = mappend acc <$> energyToMany repr (Bead beadInfo) (neighbours beadInfo)
