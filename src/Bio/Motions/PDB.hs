{- |
Module      : Bio.Motions.PDB
Description : Writing simulation output to the PDB format
License:    : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Bio.Motions.PDB ( Mapping
                       , mkMapping
                       , mkSimpleMapping
                       , legacyMapping
                       , writeMapping
                       , writePDB
                       ) where

import Bio.Motions.Types
import Bio.Motions.Representation.Dump

import qualified Bio.PDB.EventParser.PDBEvents as PE
import qualified Bio.PDB.EventParser.PDBEventPrinter as PP
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import Control.Monad
import Data.MonoTraversable
import Linear
import System.IO

data Mapping = Mapping
    { beadRes :: EnergyVector -> BS.ByteString
    , binderRes :: BinderType -> BS.ByteString
    , chainId :: Int -> Char
    }

-- |Given a set of energy vectors, binder types and chain numbers creates injective mappings
-- from these sets to strings later used in the "ATOM" PDB records:
-- 1. Strings correspoding to energy vectors and binder types are of length 3 and contain
--    only the characters a-z, A-Z or 0-9. They are used in the "resName" field.
-- 2. Strings corresponding to energy vectors never start with a 'B'.
-- 3. Strings corresponding to binder types always start with a 'B'.
-- 4. Chain numbers are mapped to single characters a-z, A-Z or 0-9. They are used in the "chainID" field.
-- The provided sets must be at most as large as their counterdomains.
-- The mapping can later be used in the `writePDB` and `writeMapping` functions.
mkMapping :: [EnergyVector] -> [BinderType] -> [Int] -> Maybe Mapping
mkMapping evs bts chs = Mapping <$> mapEnergyVectors evs <*> mapBinderTypes bts <*> mapChains chs

-- |Creates a mapping that treats all binders except lamins as if they had one type.
-- The "resName" field of an "ATOM" PDB record is compatible with the prototype simulation's output.
mkSimpleMapping :: (Int -> Char) -> Mapping
mkSimpleMapping chainMap = Mapping simpleBeadRes simpleBinderRes chainMap
  where
    simpleBeadRes ev | normEV ev = "UNB"
                     | lamEV ev  = "LAM"
                     | otherwise = "BOU"
    simpleBinderRes bt | lamB bt   = "LAM"
                       | otherwise = "BIN"

    -- TODO: te funkcje by wypadało gdzieś indziej wyrzucić
    -- i zrobić coś z tymi magic numbers
    normEV ev = oall (== 0) $ getEnergyVector ev
    lamEV ev = getEnergyVector ev U.! 1 /= 0
    lamB bt = getBinderType bt == 1

-- |A mapping compatible with the prototype simulation's output.
-- Assumes that there is only one chain in the simulation.
legacyMapping :: Mapping
legacyMapping = mkSimpleMapping $ const ' '

data FrameHeader = FrameHeader
    { headerSeqNum :: Int
    , headerStep :: Int
    , headerTitle :: String
    }

-- |Outputs a mapping from energy vectors, binder types and ints to PDB compatible strings.
writeMapping :: Handle -> Mapping -> [EnergyVector] -> [BinderType] -> [Int] -> IO ()
writeMapping handle Mapping{..} evs bts chs =
    hPutStr handle . unlines $ unlines <$> [showEV <$> evs, showBT <$> bts, showCh <$> chs]
  where
    showEV ev = show ev ++ " " ++ show (beadRes ev)
    showBT bt = show bt ++ " " ++ show (binderRes bt)
    showCh ch = show ch ++ " " ++ show (chainId ch)

-- |Outputs a single simulation frame in the PDB format.
writePDB :: Handle -> FrameHeader -> Mapping -> Dump -> IO ()
writePDB handle FrameHeader{..} Mapping{..} Dump{..} =
    writeHeader >> writeData
  where
    writeHeader =
        let headerData = PE.HEADER (BS.pack . show $ headerSeqNum)
                                   (BS.pack . show $ olength binders + sum (olength <$> chains))
                                   (BS.pack $ "step " ++ show headerStep)
            titleData = PE.TITLE 0 $ BS.pack headerTitle
        in PP.print handle headerData >> PP.print handle titleData

    writeData = forM_ [beadsData, bindersData, connectsData] $ mapM_ $ PP.print handle

    beadsData = concat $ zipWith mkChainData chainSerialNums chains
    mkChainData serialOffset = zipWith3 mkBeadData [serialOffset..] [1..]

    bindersData = zipWith3 mkBinderData [olength beadsData + 1..] [1..] binders

    connectsData = concat $ zipWith (\s e -> [PE.CONECT [i, i + 1] | i <- [s..e - 2]])
                                    chainSerialNums (tail chainSerialNums)

    chainSerialNums = scanl (flip $ (+) . olength) 1 chains

    mkBinderData serialNum resSeqNum BinderInfo{..} =
        mkAtomData serialNum
                   "O" -- u nich jest jeszcze "P" w przypadku Lamin. Mogę zaifować binderType, jest sens?
                   (binderRes binderType)
                   ' '
                   resSeqNum
                   (mkCoordData binderPosition)

    mkBeadData serialNum resSeqNum BeadInfo{..} =
        mkAtomData serialNum
                   "C"
                   (beadRes beadEV)
                   (chainId beadChain)
                   resSeqNum
                   (mkCoordData beadPosition)

    mkAtomData serialNum atomType resType chainId resSeqNum coordData = PE.ATOM
        { no = serialNum
        , atomtype = atomType
        , restype = resType
        , chain = chainId
        , resid = resSeqNum
        , resins = ' '
        , altloc = ' '
        , coords = coordData
        , occupancy = 0
        , bfactor = 0
        , segid = ""
        , elt = ""
        , charge = ""
        , hetatm = False
        }

    mkCoordData (V3 x y z) = 3 * PE.Vector3 (fromIntegral x) (fromIntegral y) (fromIntegral z)

chars :: [Char]
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- |Creates an injective mapping from a set of EnergyVectors to ByteStrings of length 3
-- with possible characters a-z, A-Z or 0-9 not starting with a 'B'. The set must be
-- at most as large as the counterdomain.
mapEnergyVectors :: [EnergyVector] -> Maybe (EnergyVector -> BS.ByteString)
mapEnergyVectors evs = guard (length evs <= length vals) >> return ((M.!) mapping)
  where vals = [BS.pack [a, b, c] | [a, b, c] <- replicateM 3 chars, a /= 'B']
        mapping = M.fromList $ zip evs vals


-- |Creates an injective mapping from a set of BinderTypes to ByteStrings of length 3
-- with possible characters a-z, A-Z or 0-9 starting with a 'B'. The set must be
-- at most as large as the counterdomain.
mapBinderTypes :: [BinderType] -> Maybe (BinderType -> BS.ByteString)
mapBinderTypes bts = guard (length bts <= length vals) >> return ((M.!) mapping)
  where vals = [BS.pack ['B', a, b] | [a, b] <- replicateM 2 chars]
        mapping = M.fromList $ zip bts vals

-- |Creates an injective mapping from a set of Ints (representing chain numbers) to Chars.
-- The possible characters are: a-z, A-Z, 0-9. The set must be at most as large as the counterdomain.
mapChains :: [Int] -> Maybe (Int -> Char)
mapChains chs = guard (length chs <= length chars) >> return ((M.!) mapping)
  where mapping = M.fromList $ zip chs chars
