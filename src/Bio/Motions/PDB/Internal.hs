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

data FrameHeader = FrameHeader
    { headerSeqNum :: Int
    , headerStep :: Int
    , headerTitle :: String
    }

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

data PDBEntry = PDBHeader  { seqNum :: Int
                           , atomCount :: Int
                           , step :: Int
                           }
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
    step meta entry = case entry of
                        EnergyVectorMap ev str -> updateEV meta str ev
                        BinderTypeMap bt str -> updateBT meta str bt
                        ChainIdMap ch str -> updateCH meta str ch
    updateEV meta str ev = if M.member str (fromBeadRes meta)
                              then Left "Duplicate energy vector strings"
                              else Right meta { fromBeadRes = M.insert str ev $ fromBeadRes meta }
    updateBT meta str bt = if M.member str (fromBinderRes meta)
                              then Left "Duplicate binder type strings"
                              else Right meta { fromBinderRes = M.insert str bt $ fromBinderRes meta }
    updateCH meta str ch = if M.member str (fromChainId meta)
                              then Left "Duplicate chain id strings"
                              else Right meta { fromChainId = M.insert str ch $ fromChainId meta }

toPDBData :: FrameHeader -> PDBMeta -> Dump -> [PDBEntry]
toPDBData FrameHeader{..} meta Dump{..} =
    [headerData, titleData] ++ beadsData ++ bindersData ++ connectsData
  where
    headerData = PDBHeader headerSeqNum (length dumpBinders + sum (length <$> dumpChains)) headerStep
    titleData = PDBTitle headerTitle
    beadsData = concat $ zipWith (toChainData meta) chainSerials $ addIndices dumpChains
    bindersData = zipWith3 (toBinderData meta) [length beadsData + 1..] [1..] dumpBinders
    connectsData = concat $ zipWith (\s e -> toConnectData <$> [s..e - 2]) <*> tail $ chainSerials
    chainSerials = scanl (flip $ (+) . length) 1 dumpChains

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
fromPDBData meta es =
    let (binderEntries, es') = partition isBinder es
        (beadEntries, es'') = partition isBead es'
        (connectEntries, _) = partition isConnect es'' -- TODO: check the rest?
    in do
        dumpBinders <- mapM (fromBinderData meta) binderEntries
        (forMap, revMap) <- fromConnectsData connectEntries
        beadMap <- M.fromList <$> mapM (\e -> (serial e, ) <$> fromBeadData meta e) beadEntries
        dumpChains <- map snd <$> toChains forMap revMap beadMap -- TODO: chain ids
        let dumpRadius = 10 --TODO
        pure Dump{..}

  where
    isBinder PDBAtom{..} = name == "C"
    isBinder _           = False
    isBead PDBAtom{..} = name `elem` ["L", "O"]
    isBead _           = False
    isConnect PDBConnect{} = True
    isConnect _            = False

fromBinderData :: RevPDBMeta -> PDBEntry -> Either ReadError BinderInfo
fromBinderData RevPDBMeta{..} PDBAtom{..} = BinderInfo pos <$> bt
  where
    pos = fromCoordData coords
    bt = M.findWithDefault (Left err) resName (Right <$> fromBinderRes)
    err = "Unknown binder residue name: " ++ resName

fromBeadData :: RevPDBMeta -> PDBEntry -> Either ReadError (ChainId, DumpBeadInfo)
fromBeadData RevPDBMeta{..} PDBAtom{..} = (,) <$> ch <*> (DumpBeadInfo pos <$> ev)
  where
    pos = fromCoordData coords
    ev = M.findWithDefault (Left resErr) resName (Right <$> fromBeadRes)
    ch = M.findWithDefault (Left chErr) chainID (Right <$> fromChainId)
    resErr = "Unknown bead residue name: " ++ resName
    chErr = "Unknown chainID: " ++ show chainID

fromConnectsData :: [PDBEntry] -> Either ReadError (M.Map Serial Serial, M.Map Serial Serial)
fromConnectsData = foldM step (M.empty, M.empty)
  where
    step (forMap, revMap) PDBConnect{..} = do
        when (fstSerial `M.member` forMap) $ throwError $ err fstSerial sndSerial forMap
        when (sndSerial `M.member` revMap) $ throwError $ err sndSerial fstSerial revMap
        pure (M.insert fstSerial sndSerial forMap, M.insert sndSerial fstSerial revMap)
    err k a m = "Atom " ++ show k ++ " connects to both " ++ show a ++ " and " ++ show (M.lookup k m)

fromCoordData :: V3 Double -> Vec3
fromCoordData = fmap round . (/ 3)

-- |Creates a list of chains tagged with their ids.
toChains ::
    M.Map Serial Serial -- ^A bijection between a subset of beads representing connections between them.
 -> M.Map Serial Serial -- ^The bijection reverse to the first argument.
 -> M.Map Serial (ChainId, DumpBeadInfo) -- ^A set of beads tagged with their PDB serial numbers.
 -> Either ReadError [(ChainId, [DumpBeadInfo])]
toChains forMap revMap = go
  where
    go m = if M.null m then Right [] else do
        let (serial, (chId, _)) = M.findMin m
        start <- findStart serial serial
        (chain, m') <- goChain start chId start m
        ((chId, chain) :) <$> go m'
    goChain start chId cur m = do
        (chId', bead) <- maybe (Left ("Non-existing atom: " ++ show cur)) Right $ M.lookup cur m
        when (chId /= chId') $ throwError $ errChId start chId cur chId'
        let m' = M.delete cur m
        case M.lookup cur forMap of
          Nothing -> Right ([bead], m')
          Just next -> first (bead :) <$> goChain start chId next m'
    findStart start cur =
        case M.lookup cur revMap of
          Nothing -> Right cur
          Just next | next == start -> Left $ errCycle start
          Just next -> findStart start next
    errChId a1 ch1 a2 ch2 = "Atom " ++ show a1 ++ " from chain " ++ show ch1 ++ "is connected to atom "
                                    ++ show a2 ++ " from different chain " ++ show ch2
    errCycle a = "Atom " ++ show a ++ " lies on a connect cycle"

-- |Characters used in string/character fields of PDB entries.
pdbChars :: [Char]
pdbChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
