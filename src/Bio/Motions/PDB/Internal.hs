{- |
Module      : Bio.Motions.PDB.Internal
Description : Internal definitions for handling the PDB format.
License:    : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Bio.Motions.PDB.Internal where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Parsec
import Bio.Motions.Utils.Common

import Control.Lens
import Control.Monad
import Control.Monad.State as S
import Control.Monad.Except
import Control.Arrow
import GHC.Exts
import Linear
import Text.ParserCombinators.Parsec as P
import Data.List
import qualified Data.Map as M
import qualified Bio.PDB.EventParser.PDBEvents as PE
import qualified Bio.PDB.EventParser.PDBEventParser as PP
import qualified Data.ByteString.Char8 as BS
import System.IO

type Serial = Int
type ReadError = String

data FrameHeader =
    StepHeader { headerSeqNum :: Int
               , headerStep :: Int
               , headerTitle :: String
               }
  | LaminHeader

data PDBMeta = PDBMeta
    { beadRes :: M.Map EnergyVector String
    -- ^ Maps EnergyVectors to strings later used in the @resName@ field of a PDB @ATOM@ entry.
    --   The resulting strings are of length 3, contain only the characters defined in 'pdbChars'
    --   and do not start with a @B@.
    , binderRes :: M.Map BinderType String
    -- ^ Maps BinderTypes to strings later used in the @resName@ field of a PDB @ATOM@ entry.
    --   The resulting strings are of length 3, contain only the characters defined in 'pdbChars'
    --   and start with a @B@.
    , chainId :: M.Map ChainId Char
    -- ^ Maps chain numbers to characters (only those defined in 'pdbChars') later used in the
    --   @chainID@ field of a PDB @ATOM@ entry.
    }

data RevPDBMeta = RevPDBMeta
    { fromBeadRes :: M.Map String EnergyVector
    , fromBinderRes :: M.Map String BinderType
    , fromChainId :: M.Map Char ChainId
    }

data PDBEntry = PDBHeader  { classification :: String }
              | PDBTitle   { title :: String }
              | PDBAtom    { serial :: Serial    -- ^ Atom serial number
                           , name :: String      -- ^ Atom name
                           , resName :: String   -- ^ Residue name
                           , chainID :: Char     -- ^ Chain identifier
                           , resSeq :: Int       -- ^ Residue sequence number
                           , coords :: V3 Double -- ^ Coordinates (X, Y, Z) in Angstroms
                           } -- The other ATOM fields (occupancy, tempFactor etc.) are always 0 or empty.
              | PDBConnect { fstSerial :: Serial
                           , sndSerial :: Serial
                           }
    deriving Show

data PDBMetaEntry = EnergyVectorMap EnergyVector String
                  | BinderTypeMap BinderType String
                  | ChainIdMap ChainId Char

instance Show PDBMetaEntry where
    show (EnergyVectorMap ev str) = "EV " ++ (show . toList) ev ++ " " ++ str
    show (BinderTypeMap bt str) = "BT " ++ (show . getBinderType) bt ++ " " ++ str
    show (ChainIdMap ch c) = "CH " ++ show ch ++ " " ++ [c]

readPDBMetaData :: Handle -> IO (Either ReadError [PDBMetaEntry])
readPDBMetaData h = parsePDBMetaData <$> hGetContents h

parsePDBMetaData :: String -> Either ReadError [PDBMetaEntry]
parsePDBMetaData = left show . runParser metaEntries () ""
  where
    metaEntries :: Parser [PDBMetaEntry]
    metaEntries = metaEntry `sepBy` eol

    metaEntry :: Parser PDBMetaEntry
    metaEntry = EnergyVectorMap <$> (string "EV" *> spaces *> energyVector <* spaces)
                                <*> (energyVectorString <* spaces)
            <|> BinderTypeMap <$> (string "BT" *> spaces *> binderType <* spaces)
                              <*> (binderTypeString <* spaces)
            <|> ChainIdMap <$> (string "CH" *> spaces *> int <* spaces)
                           <*> (oneOf pdbChars <* spaces)

    binderTypeString :: Parser String
    binderTypeString = char 'B' >> replicateM 2 (oneOf pdbChars)

    binderType :: Parser BinderType
    binderType = BinderType <$> int

    energyVectorString :: Parser String
    energyVectorString = oneOf (pdbChars \\ ['B']) >> replicateM 2 (oneOf pdbChars)

toPDBMetaData :: PDBMeta -> [PDBMetaEntry]
toPDBMetaData PDBMeta{..} = map (uncurry EnergyVectorMap) (toList beadRes)
                         ++ map (uncurry BinderTypeMap) (toList binderRes)
                         ++ map (uncurry ChainIdMap) (toList chainId)

toRevPDBMeta :: [PDBMetaEntry] -> Either ReadError RevPDBMeta
toRevPDBMeta = foldM step $ RevPDBMeta M.empty M.empty M.empty
  where
    step meta (EnergyVectorMap ev str)
      | M.member str (fromBeadRes meta) = Left "Duplicate energy vector strings"
      | otherwise = Right meta { fromBeadRes = M.insert str ev $ fromBeadRes meta }
    step meta (BinderTypeMap bt str)
      | M.member str (fromBinderRes meta) = Left "Duplicate binder type strings"
      | otherwise = Right meta { fromBinderRes = M.insert str bt $ fromBinderRes meta }
    step meta (ChainIdMap ch str)
      | M.member str (fromChainId meta) = Left "Duplicate chain id strings"
      | otherwise = Right meta { fromChainId = M.insert str ch $ fromChainId meta }

toPDBData :: FrameHeader -> PDBMeta -> Dump -> [PDBEntry]
toPDBData header meta dump@Dump{..} =
    headerData ++ beadsData ++ bindersData ++ connectsData
  where
    headerData = toHeaderData header dump
    beadsData = concat $ zipWith (toChainData meta) chainSerials $ addIndices dumpChains
    bindersData = zipWith3 (toBinderData meta) [length beadsData + 1..] [1..] dumpBinders
    connectsData = concat $ zipWith (\s e -> toConnectData <$> [s..e - 2]) <*> tail $ chainSerials
    chainSerials = scanl (flip $ (+) . length) 1 dumpChains

toHeaderData :: FrameHeader -> Dump -> [PDBEntry]
toHeaderData StepHeader{..} Dump{..} =
    [ PDBHeader (show headerSeqNum ++ " " ++ show atomCount ++ " step " ++ show headerStep)
    , PDBTitle headerTitle ]
  where atomCount = length dumpBinders + sum (map length dumpChains)
toHeaderData LaminHeader _ = [PDBHeader "LAMINA"]

toBinderData :: PDBMeta -> Serial -> Int -> BinderInfo -> PDBEntry
toBinderData PDBMeta{..} serial resSeq binder =
    PDBAtom { serial = serial
            , name = if binder ^. binderType == laminType then "L" else "O"
            , resName = binderRes M.! (binder ^. binderType)
            , chainID = ' '
            , resSeq = resSeq
            , coords = toCoordData $ binder ^. position
            }

toChainData :: PDBMeta -> Int -> [BeadInfo] -> [PDBEntry]
toChainData meta serialOffset = zipWith3 (toBeadData meta) [serialOffset..] [1..]

toBeadData :: PDBMeta -> Serial -> Int -> BeadInfo -> PDBEntry
toBeadData PDBMeta{..} serial resSeq bead =
    PDBAtom { serial = serial
            , name = "C"
            , resName = beadRes M.! (bead ^. beadEV)
            , chainID = chainId M.! (bead ^. beadChain)
            , resSeq = resSeq
            , coords = toCoordData $ bead ^. position
            }

toCoordData :: Vec3 -> V3 Double
toCoordData = (* 3) . fmap fromIntegral

toConnectData :: Serial -> PDBEntry
toConnectData i = PDBConnect i (i + 1)

fromPDBData :: RevPDBMeta -> [PDBEntry] -> Either ReadError Dump
fromPDBData meta es = do
    dumpBinders <- mapM (fromBinderData meta) binderEntries
    connects <- fromConnectsData connectEntries
    beadMap <- M.fromList <$> mapM (\e -> (serial e, ) <$> fromBeadData meta e) beadEntries
    dumpChains <- map snd <$> extractChains connects beadMap -- TODO: chain ids
    pure Dump{..}
  where
    binderEntries = [e | e@PDBAtom{..} <- es, name == "C"]
    beadEntries = [e | e@PDBAtom{..} <- es, name `elem` ["L", "O"]]
    connectEntries = [e | e@PDBConnect{} <- es]

fromBinderData :: RevPDBMeta -> PDBEntry -> Either ReadError BinderInfo
fromBinderData RevPDBMeta{..} PDBAtom{..} = BinderInfo pos <$> bt
  where
    pos = fromCoordData coords
    bt = findOrError err resName fromBinderRes
    err = "Unknown binder residue name: " ++ resName

fromBeadData :: RevPDBMeta -> PDBEntry -> Either ReadError (ChainId, DumpBeadInfo)
fromBeadData RevPDBMeta{..} PDBAtom{..} = (,) <$> ch <*> (DumpBeadInfo pos <$> ev)
  where
    pos = fromCoordData coords
    ev = findOrError resErr resName fromBeadRes
    ch = findOrError chErr chainID fromChainId
    resErr = "Unknown bead residue name: " ++ resName
    chErr = "Unknown chainID: " ++ show chainID

fromConnectsData :: [PDBEntry] -> Either ReadError (M.Map Serial Serial, M.Map Serial Serial)
fromConnectsData = foldM step (M.empty, M.empty)
  where
    step (forMap, revMap) PDBConnect{..} = do
        when (fstSerial `M.member` forMap) $ throwError $ err fstSerial sndSerial forMap
        when (sndSerial `M.member` revMap) $ throwError $ err sndSerial fstSerial revMap
        pure (M.insert fstSerial sndSerial forMap, M.insert sndSerial fstSerial revMap)
    err k a m = "Atom " ++ show k ++ " is connected to both " ++ show a ++ " and " ++ show (M.lookup k m)

fromCoordData :: V3 Double -> Vec3
fromCoordData = fmap round . (/ 3)

-- |Extracts all chains tagged with their IDs from a set of beads.
extractChains ::
     (M.Map Serial Serial, M.Map Serial Serial)
  -- ^A bijection between a subset of the set of beads, representing connections between them.
  -> M.Map Serial (ChainId, DumpBeadInfo)
  -- ^The set of beads tagged with their PDB serial numbers.
  -> Either ReadError [(ChainId, [DumpBeadInfo])]
  -- ^The resulting list of chains or error if the input was invalid.
extractChains (forMap, revMap) = go
  where
    go m | M.null m = Right []
         | otherwise = do
        let (serial, (chId, _)) = M.findMin m
        start <- findStart serial serial
        (chain, m') <- extractOneChain forMap start chId m
        ((chId, chain) :) <$> go m'
    findStart start cur =
        case M.lookup cur revMap of
          Nothing -> pure cur
          Just next | next == start -> throwError $ "Atom " ++ show start ++ " lies on a connect cycle"
          Just next -> findStart start next

-- |Extracts one chain from a set of beads.
extractOneChain ::
     M.Map Serial Serial
  -- ^A bijection between a subset of the set of beads, representing connections between them.
  -> Serial
  -- ^The PDB serial number of the beginning of the chain.
  -> ChainId
  -- ^The ID of the chain.
  -> M.Map Serial (ChainId, DumpBeadInfo)
  -- ^The set of beads tagged with their PDB serial numbers.
  -> Either ReadError ([DumpBeadInfo], M.Map Serial (ChainId, DumpBeadInfo))
  -- ^The resulting chain and the set of remaining beads or error if the input was invalid.
extractOneChain connects start chId = go start
  where
    go cur m = do
        (chId', bead) <- findOrError ("Non-existing atom: " ++ show cur) cur m
        when (chId /= chId') $ throwError $
            "Atom " ++ show start ++ " from chain " ++ show chId ++ " is connected to atom "
            ++ show cur ++ " from different chain " ++ show chId'
        let m' = M.delete cur m
        case M.lookup cur connects of
          Nothing -> pure ([bead], m')
          Just next -> first (bead :) <$> go next m'

-- |Characters used in string/character fields of PDB entries.
pdbChars :: [Char]
pdbChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
