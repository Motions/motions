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

import Data.ByteString.Builder
import Data.Monoid
import System.IO (Handle)
import Linear

writePDB :: Handle -> FrameHeader -> PDBMeta -> Dump -> IO ()
writePDB h f m = writePDBData h . toPDBData f m

writePDBData :: Handle -> [PDBEntry] -> IO ()
writePDBData h es = hPutBuilder h . mconcat $ [ toBuilder e' <> char8 '\n' | e' <- es ]

toBuilder :: PDBEntry -> Builder
toBuilder PDBHeader{..} = "HEADER " <> string8 classification
toBuilder PDBTitle{..}  = "TITLE " <> string8 title
toBuilder PDBAtom{..} =
       "ATOM  "
    <> rightInt 5 serial
    <> space
    <> space <> string8 (justifyLeft 3 name) -- not PDB format but compatible with python
    <> space
    <> string8 resName
    <> space
    <> char8 chainID
    <> rightInt 4 resSeq
    <> padding 4
    <> rightInt 4 x <> ".000"
    <> rightInt 4 y <> ".000"
    <> rightInt 4 z <> ".000"
    <> "  0.00"
    <> "  0.00"
  where V3 x y z = coords
toBuilder PDBConnect{..} =
       "CONECT"
    <> rightInt 5 fstSerial
    <> rightInt 5 sndSerial

justifyLeft :: Int -> String -> String
justifyLeft n s = s ++ replicate (n - length s) ' '
{-# INLINE justifyLeft #-}

rightInt :: Int -> Int -> Builder
rightInt just x = padding n <> intDec x
  where
    n = just - intLen x
    intLen y = intLen' y 1
    intLen' y acc | y < 0 = intLen' (-y) (acc + 1)
    intLen' y acc | y <= 9 = acc
    intLen' y acc = intLen' (y `div` 10) (acc + 1)
{-# INLINE rightInt #-}

padding :: Int -> Builder
padding n = mconcat . replicate n $ space
{-# INLINE padding #-}

space :: Builder
space = word8 . toEnum . fromEnum $ ' '
