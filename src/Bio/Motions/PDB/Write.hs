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

import Bio.Motions.Types
import Bio.Motions.PDB.Internal
import Bio.Motions.Representation.Dump

import qualified Bio.PDB.EventParser.PDBEvents as PE
import qualified Bio.PDB.EventParser.PDBEventPrinter as PP
import qualified Data.ByteString.Char8 as BS
import System.IO
import Linear

writePDB :: Handle -> FrameHeader -> PDBMeta -> Dump -> IO ()
writePDB h f m = writePDBData h . toPDBData f m

writePDBData :: Handle -> [PDBEntry] -> IO ()
writePDBData handle = mapM_ $ PP.print handle . toEvent

toEvent :: PDBEntry -> PE.PDBEvent
toEvent PDBHeader{..}  = PE.HEADER (BS.pack classification) "" ""
toEvent PDBTitle{..}   = PE.TITLE 0 $ BS.pack title
toEvent PDBAtom{..} = PE.ATOM
    { no = serial
    , atomtype = BS.pack name
    , restype = BS.pack resName
    , chain = chainID
    , resid = resSeq
    , resins = ' '
    , altloc = ' '
    , coords = case coords of V3 x y z -> PE.Vector3 x y z
    , occupancy = 0
    , bfactor = 0
    , segid = ""
    , elt = ""
    , charge = ""
    , hetatm = False
    }
toEvent PDBConnect{..} = PE.CONECT [fstSerial, sndSerial]
