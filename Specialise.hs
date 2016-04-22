{- |
Module      : Specialise
Description : Contains specialisations of simulate for commonly used parameters
License     : Apache
Stability   : experimental
Portability : unportable
 -}
module Specialise where

import Bio.Motions.Callback.StandardScore
import qualified Bio.Motions.Engine as E
import Bio.Motions.Format.Handle
import Bio.Motions.PDB.Backend
import Bio.Motions.Representation.Chain
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random
import GHC.Exts

simulate'IOChain'StandardScore'PDB'MWCIO :: E.RunSettings IOChainRepresentation StandardScore PDBBackend
                                            -> Dump -> MWCIO Dump
simulate'IOChain'StandardScore'PDB'MWCIO = inline E.simulate
{-# NOINLINE simulate'IOChain'StandardScore'PDB'MWCIO #-}

simulate'IOChain'StandardScore'Bin'MWCIO :: E.RunSettings IOChainRepresentation StandardScore BinaryBackend
                                            -> Dump -> MWCIO Dump
simulate'IOChain'StandardScore'Bin'MWCIO = inline E.simulate
{-# NOINLINE simulate'IOChain'StandardScore'Bin'MWCIO #-}
