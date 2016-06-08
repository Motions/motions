{- |
Module      : Bio.Motions.PDB.Write
Description : Writing simulation output to the PDB format.
License:    : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Bio.Motions.PDB.Write ( writePDB
                             , FrameHeader(..)
                             ) where

import Bio.Motions.PDB.Internal
import Bio.Motions.Representation.Dump

import System.IO
import Linear
import Data.ByteString.Builder as BB
import Data.ByteString.Builder.Extra
import qualified Data.ByteString.Lazy as BL
import Data.Monoid

writePDB :: Handle -> FrameHeader -> PDBMeta -> Dump -> IO ()
writePDB h f m = writePDBData h . toPDBData f m

writePDBData :: Handle -> [PDBEntry] -> IO ()
writePDBData handle =
    BL.hPut handle . toLazyByteStringWith (untrimmedStrategy smallChunkSize defaultChunkSize) BL.empty .
        mconcat . map toBuilder
{-# INLINE writePDBData #-}

toBuilder :: PDBEntry -> Builder
toBuilder PDBHeader{..}  = string8 "HEADER" <> string8 classification
toBuilder PDBTitle{..}   = string8 "TITLE" <> intDec 0 <> string8 title
toBuilder PDBAtom{..} = 
      intDec serial
    <> string8 name
    <> string8 resName
    <> BB.char8 chainID
    <> intDec resSeq
    <> BB.char8 ' '
    <> BB.char8 ' '
    <> pos
    <> intDec 0
    <> intDec 0
    <> string8 ""
    <> string8 ""
    <> string8 ""
  where
      pos = case coords of V3 x y z -> intDec (truncate x) <> intDec (truncate y) <> intDec (truncate z)
      {-# INLINE pos #-}

toBuilder PDBConnect{..} = string8 "CONECT" <> intDec fstSerial <> intDec sndSerial
{-# INLINE toBuilder #-}
