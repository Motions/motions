{- |
Module      : Bio.Motions.PDB.Internal
Description : Internal definitions for handling the PDB format.
License:    : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.PDB.Internal where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Dump

import Data.MonoTraversable
import Linear

data FrameHeader = FrameHeader
    { headerSeqNum :: Int
    , headerStep :: Int
    , headerTitle :: String
    }

data PDBMeta = PDBMeta
    { beadRes :: EnergyVector -> String
    -- ^ Maps EnergyVectors to strings later used in the @resName@ field of a PDB @ATOM@ entry.
    --   The resulting strings are of length 3, contain only the characters defined in 'chars'
    --   and do not start with a @B@.
    , binderRes :: BinderType -> String
    -- ^ Maps BinderTypes to strings later used in the @resName@ field of a PDB @ATOM@ entry.
    --   The resulting strings are of length 3, contain only the characters defined in 'chars'
    --   and start with a @B@.
    , chainId :: Int -> Char
    -- ^ Maps chain numbers to characters (only those defined in 'chars') later used in the
    --   @chainID@ field of a PDB @ATOM@ entry.
    }

data PDBEntry = PDBHeader  { seqNum :: Int
                           , atomCount :: Int
                           , step :: Int
                           }
              | PDBTitle   { title :: String }
              | PDBAtom    { serial :: Int       -- ^ Atom serial number
                           , name :: String      -- ^ Atom name
                           , resName :: String   -- ^ Residue name
                           , chainID :: Char     -- ^ Chain identifier
                           , resSeq :: Int       -- ^ Residue sequence number
                           , coords :: V3 Double -- ^ Coordinates (X, Y, Z) in Angstroms
                           } -- The other ATOM fields (occupancy, tempFactor etc.) are always 0 or empty.
              | PDBConnect { fstSerial :: Int
                           , sndSerial :: Int
                           }

data PDBMetaEntry = EnergyVectorMap EnergyVector String
                  | BinderTypeMap BinderType String
                  | ChainIdMap ChainId Char

instance Show PDBMetaEntry where
    show (EnergyVectorMap ev str) = "EV" ++ (show . otoList . getEnergyVector) ev ++ " " ++ str
    show (BinderTypeMap bt str) = "BT" ++ show bt ++ " " ++ str
    show (ChainIdMap ch c) = "CH" ++ show ch ++ " " ++ [c]

toPDBMetaData :: [EnergyVector] -> [BinderType] -> [Int] -> PDBMeta -> [PDBMetaEntry]
toPDBMetaData evs bts chs PDBMeta{..} = fmap (EnergyVectorMap <*> beadRes) evs
                                     ++ fmap (BinderTypeMap <*> binderRes) bts
                                     ++ fmap (ChainIdMap <*> chainId) chs


toPDBData :: FrameHeader -> PDBMeta -> Dump -> [PDBEntry]
toPDBData FrameHeader{..} meta Dump{..} =
    [headerData, titleData] ++ beadsData ++ bindersData ++ connectsData
  where
    headerData = PDBHeader headerSeqNum (olength binders + sum (olength <$> chains)) headerStep
    titleData = PDBTitle headerTitle
    beadsData = concat $ zipWith (toChainData meta) chainSerials chains
    bindersData = zipWith3 (toBinderData meta) [olength beadsData + 1..] [1..] binders
    connectsData = concat $ zipWith (\s e -> [toConnectData i | i <- [s..e - 2]])
                                    chainSerials (tail chainSerials)
    chainSerials = scanl (flip $ (+) . olength) 1 chains

toBinderData :: PDBMeta -> Int -> Int -> BinderInfo -> PDBEntry
toBinderData PDBMeta{..} serial resSeq BinderInfo{..} =
    PDBAtom { serial = serial
            , name = if binderType == laminType then "L" else "O"
            , resName = binderRes binderType
            , chainID = ' '
            , resSeq = resSeq
            , coords = toCoordData binderPosition
            }

toChainData :: PDBMeta -> Int -> [BeadInfo] -> [PDBEntry]
toChainData meta serialOffset = zipWith3 (toBeadData meta) [serialOffset..] [1..]

toBeadData :: PDBMeta -> Int -> Int -> BeadInfo -> PDBEntry
toBeadData PDBMeta{..} serial resSeq BeadInfo{..} =
    PDBAtom { serial = serial
            , name = "C"
            , resName = beadRes beadEV
            , chainID = chainId beadChain
            , resSeq = resSeq
            , coords = toCoordData beadPosition
            }

toCoordData :: Vec3 -> V3 Double
toCoordData = (* 3) . fmap fromIntegral

toConnectData :: Int -> PDBEntry
toConnectData i = PDBConnect i (i + 1)
