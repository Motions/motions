{- |
Module      : Specialise
Description : Contains specialisations of simulate for commonly used parameters
License     : Apache
Stability   : experimental
Portability : unportable
 -}
module Specialise where

import Bio.Motions.Callback.StandardScore
import Bio.Motions.Engine
import Bio.Motions.Format.Backend.Writer
import Bio.Motions.PDB.Backend.Writer
import Bio.Motions.Input
import Bio.Motions.Representation.Chain
import Bio.Motions.Representation.Chain.Slow
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random
import GHC.Exts
import GHC.TypeLits

{- We want to force the GHC to specialise the 'simulate' function for common sets of parameters,
 - vastly improving performance. However, it seems that SPECIALISE pragmas alone are not sufficient,
 - GHC ignores them in the "Main" module, despite the specialisable code being present in the generated
 - Core. As a workaround, we use explicit rewrite rules to replace this code with its specialisation.
 - However, those RULES would also apply to the definition of the specialised function. Therefore,
 - we define the specialisations in a separate module.
 -}
simulate'IOChain'StandardScore'PDB'MWCIO :: RunSettings IOChainRepresentation StandardScore PDBWriter MoveGenerator
                                            -> Dump -> MWCIO Dump
simulate'IOChain'StandardScore'PDB'MWCIO = inline simulate
{-# NOINLINE simulate'IOChain'StandardScore'PDB'MWCIO #-}

simulate'IOChain'StandardScore'Bin'MWCIO :: RunSettings IOChainRepresentation StandardScore BinaryWriter MoveGenerator
                                            -> Dump -> MWCIO Dump
simulate'IOChain'StandardScore'Bin'MWCIO = inline simulate
{-# NOINLINE simulate'IOChain'StandardScore'Bin'MWCIO #-}

simulate'SlowChain'StandardScore'PDB'MWCIO :: (KnownNat r, KnownNat d)
       => RunSettings (SlowChainRepresentation r d) StandardScore PDBWriter MoveGenerator -> Dump -> MWCIO Dump
simulate'SlowChain'StandardScore'PDB'MWCIO = inline simulate
{-# NOINLINE simulate'SlowChain'StandardScore'PDB'MWCIO #-}

simulate'SlowChain'StandardScore'Bin'MWCIO :: (KnownNat r, KnownNat d)
       => RunSettings (SlowChainRepresentation r d) StandardScore BinaryWriter MoveGenerator -> Dump -> MWCIO Dump
simulate'SlowChain'StandardScore'Bin'MWCIO = inline simulate
{-# NOINLINE simulate'SlowChain'StandardScore'Bin'MWCIO #-}
