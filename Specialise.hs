{- |
Module      : Specialise
Description : Contains specialisations of simulate for commonly used parameters
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Specialise where

import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius()
import LoadCallbacks()

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Discover
import Bio.Motions.Engine
import Bio.Motions.Format.Handle
import Bio.Motions.PDB.Backend
import Bio.Motions.Representation.Chain
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random
import GHC.Exts

type PreCallbacks = $(allCallbacks Pre)
type PostCallbacks = $(allCallbacks Post)

{- We want to force the GHC to specialise the 'simulate' function for common sets of parameters,
 - vastly improving performance. However, it seems that SPECIALISE pragmas alone are not sufficient,
 - GHC ignores them in the "Main" module, despite the specialisable code being present in the generated
 - Core. As a workaround, we use explicit rewrite rules to replace this code with its specialisation.
 - However, those RULES would also apply to the definition of the specialised function. Therefore,
 - we define the specialisations in a separate module.
 -}
simulate'IOChain'StandardScore'PDB'MWCIO :: RunSettings IOChainRepresentation StandardScore PDBBackend PreCallbacks PostCallbacks
                                            -> Dump -> MWCIO Dump
simulate'IOChain'StandardScore'PDB'MWCIO = inline simulate
{-# NOINLINE simulate'IOChain'StandardScore'PDB'MWCIO #-}

simulate'IOChain'StandardScore'Bin'MWCIO :: RunSettings IOChainRepresentation StandardScore BinaryBackend PreCallbacks PostCallbacks
                                            -> Dump -> MWCIO Dump
simulate'IOChain'StandardScore'Bin'MWCIO = inline simulate
{-# NOINLINE simulate'IOChain'StandardScore'Bin'MWCIO #-}
