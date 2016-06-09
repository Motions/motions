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

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import System.IO (Handle)
import Linear

writePDB :: Handle -> FrameHeader -> PDBMeta -> Dump -> IO ()
writePDB h f m = writePDBData h . toPDBData f m

writePDBData :: Handle -> [PDBEntry] -> IO ()
writePDBData h es = hPutBuilder h . mconcat $ [ toBuilder e' <> char8 '\n' | e' <- es ]

toBuilder :: PDBEntry -> Builder
toBuilder PDBHeader{..} = string8 "HEADER " <> encodeUtf8Builder classification
toBuilder PDBTitle{..}  = string8 "TITLE " <> encodeUtf8Builder title
toBuilder PDBAtom{..} =
       string8 "ATOM  "
    <> rightInt 5 serial
    <> word8 space
    <> word8 space <> encodeUtf8Builder (T.justifyLeft 3 ' ' name) -- not PDB format but compatible with python
    <> word8 space
    <> encodeUtf8Builder resName
    <> word8 space
    <> char8 chainID
    <> rightInt 4 resSeq
    <> byteString (padding 4)
    <> rightInt 4 x <> shortByteString ".000"
    <> rightInt 4 y <> shortByteString ".000"
    <> rightInt 4 z <> shortByteString ".000"
    <> "  0.00"
    <> "  0.00"
  where V3 x y z = coords
toBuilder PDBConnect{..} =
       shortByteString "CONECT"
    <> rightInt 5 fstSerial
    <> rightInt 5 sndSerial

rightInt :: Int -> Int -> Builder
rightInt just x = byteString (padding p) <> intDec x
  where
    p = just - intLen x
    intLen y = intLen' y 1
    intLen' y acc | y < 0 = intLen' (-y) (acc + 1)
    intLen' y acc | y <= 9 = acc
    intLen' y acc = intLen' (y `div` 10) (acc + 1)

padding :: Int -> BS.ByteString
padding p = BS.replicate p space

space :: Word8
space = toEnum . fromEnum $ ' '
