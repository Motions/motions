{- |
Module      : Bio.Motions.PDB.Internal
Description : Internal definitions for handling the PDB format.
License:    : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Bio.Motions.PDB.Internal where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Common
import Bio.Motions.Utils.Parsec
import Bio.Motions.Utils.Common

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Arrow
import GHC.Exts
import Linear
import Text.Parsec
import Text.Parsec.ByteString
import Data.List
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS

type Serial = Int
type ReadError = String

data FrameHeader =
    StepHeader { headerSeqNum :: Int
               , headerStep :: StepCounter
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
    { revBeadRes :: M.Map String EnergyVector
    , revBinderRes :: M.Map String BinderType
    , revChainId :: M.Map Char ChainId
    }
    deriving (Eq, Show)

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

printPDBMetaEntry :: PDBMetaEntry -> String
printPDBMetaEntry (EnergyVectorMap ev str) = "EV " ++ (show . toList) ev ++ " " ++ str
printPDBMetaEntry (BinderTypeMap bt str) = "BT " ++ (show . getBinderType) bt ++ " " ++ str
printPDBMetaEntry (ChainIdMap ch c) = "CH " ++ show ch ++ " " ++ [c]

parsePDBMetaData :: BS.ByteString -> Either ReadError [PDBMetaEntry]
parsePDBMetaData = parseOrError metaEntries
  where
    metaEntries :: Parser [PDBMetaEntry]
    metaEntries = sepEndBy metaEntry endOfLine <* eof

    metaEntry :: Parser PDBMetaEntry
    metaEntry = energyVecMap <|> binderTypeMap <|> chainIdMap

    energyVecMap :: Parser PDBMetaEntry
    energyVecMap = EnergyVectorMap <$> (string "EV" *> spaces *> energyVector <* spaces)
                                   <*> energyVectorString
                                   <?> "Energy vector mapping"

    binderTypeMap :: Parser PDBMetaEntry
    binderTypeMap = BinderTypeMap <$> (string "BT" *> spaces *> binderType <* spaces)
                                  <*> binderTypeString
                                  <?> "Binder type mapping"

    chainIdMap :: Parser PDBMetaEntry
    chainIdMap = ChainIdMap <$> (string "CH" *> spaces *> int <* spaces)
                            <*> oneOf pdbChars
                            <?> "Chain id mapping"

    binderTypeString :: Parser String
    binderTypeString = sequence $ char 'B' : replicate 2 (oneOf pdbChars)

    binderType :: Parser BinderType
    binderType = BinderType <$> int

    energyVectorString :: Parser String
    energyVectorString = sequence $ oneOf (pdbChars \\ ['B']) : replicate 2 (oneOf pdbChars)

toPDBMetaData :: PDBMeta -> [PDBMetaEntry]
toPDBMetaData PDBMeta{..} = map (uncurry EnergyVectorMap) (toList beadRes)
                         ++ map (uncurry BinderTypeMap) (toList binderRes)
                         ++ map (uncurry ChainIdMap) (toList chainId)

toRevPDBMeta :: [PDBMetaEntry] -> Either ReadError RevPDBMeta
toRevPDBMeta = foldM step (RevPDBMeta M.empty M.empty M.empty) >=> \meta -> validate meta >> pure meta
  where
    step meta (EnergyVectorMap ev str)
      | M.member str (revBeadRes meta) = throwError $ "Duplicate energy vector strings: " ++ str
      | otherwise = pure meta { revBeadRes = M.insert str ev $ revBeadRes meta }
    step meta (BinderTypeMap bt str)
      | M.member str (revBinderRes meta) = throwError $ "Duplicate binder type strings: " ++ str
      | otherwise = pure meta { revBinderRes = M.insert str bt $ revBinderRes meta }
    step meta (ChainIdMap ch c)
      | M.member c (revChainId meta) = throwError $ "Duplicate chain id chars: " ++ [c]
      | otherwise = pure meta { revChainId = M.insert c ch $ revChainId meta }

    validate RevPDBMeta{..} = do
        let evs = map getEnergyVector . M.elems $ revBeadRes
            bts = map getBinderType . M.elems $ revBinderRes
            chs = M.elems revChainId
        onDuplicates evs $ \ev -> throwError $ "Duplicate energy vectors: " ++ show ev
        onDuplicates bts $ \bt -> throwError $ "Duplicate binder types: " ++ show bt
        onDuplicates chs $ \ch -> throwError $ "Duplicate chain ids: " ++ show ch
        when (null bts || minimum bts /= 0)
            $ throwError "Mapping for binder type '0' (lamin type) is required but not provided"
        unless (and . zipWith (==) [0..] . sort $ bts)
            $ throwError "Binder types are not subsequent natural numbers"
        unless (all ((== maximum bts + 1) . length . toList) evs)
            $ throwError $ "Lengths of energy vectors do not correspond to binder types"
                           ++ " (all lengths should be equal to " ++ show (maximum bts + 1) ++ ")"

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
    [ PDBHeader $ show headerSeqNum ++ " " ++ show atomCount ++ " step " ++ show headerStep
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

fromPDBData :: RevPDBMeta -> Maybe Int -> [PDBEntry] -> Either ReadError Dump
fromPDBData meta maxd es = do
    dumpBinders <- mapM (fromBinderData meta) binderEntries
    connects <- fromConnectsData connectEntries
    beadMap <- M.fromList <$> mapM (\e -> (serial e, ) <$> fromBeadData meta e) beadEntries
    (chainIds, dumpChains) <- unzip <$> extractChains maxd connects beadMap -- TODO: chain ids in dumpChains
    onDuplicates chainIds $ \ch -> throwError $ "Two chains (not connected) with the same chain id: " ++ show ch
    pure Dump{..}
  where
    binderEntries = [e | e@PDBAtom{..} <- es, name `elem` ["L", "O"]]
    beadEntries = [e | e@PDBAtom{..} <- es, name == "C"]
    connectEntries = [e | e@PDBConnect{} <- es]

fromBinderData :: RevPDBMeta -> PDBEntry -> Either ReadError BinderInfo
fromBinderData RevPDBMeta{..} PDBAtom{..} = BinderInfo pos <$> bt
  where
    pos = fromCoordData coords
    bt = findOrError err resName revBinderRes
    err = "Unknown binder residue name: " ++ resName
fromBinderData _ _ = error "fromBinderData called with non-atom entry"

fromBeadData :: RevPDBMeta -> PDBEntry -> Either ReadError (ChainId, DumpBeadInfo)
fromBeadData RevPDBMeta{..} PDBAtom{..} = (,) <$> ch <*> (DumpBeadInfo pos <$> ev)
  where
    pos = fromCoordData coords
    ev = findOrError resErr resName revBeadRes
    ch = findOrError chErr chainID revChainId
    resErr = "Unknown bead residue name: " ++ resName
    chErr = "Unknown chainID: " ++ show chainID
fromBeadData _ _ = error "fromBeadData called with non-atom entry"

fromConnectsData :: [PDBEntry] -> Either ReadError (M.Map Serial Serial, M.Map Serial Serial)
fromConnectsData = foldM step (M.empty, M.empty)
  where
    step (forMap, revMap) PDBConnect{..} = do
        when (fstSerial `M.member` forMap) $ throwError $ err fstSerial sndSerial forMap
        when (sndSerial `M.member` revMap) $ throwError $ err sndSerial fstSerial revMap
        pure (M.insert fstSerial sndSerial forMap, M.insert sndSerial fstSerial revMap)
    step _ _ = error "fromConnectsData called with non-atom entry"
    err k a m = "Atom " ++ show k ++ " is connected to both " ++ show a ++ " and " ++ show (M.lookup k m)

fromCoordData :: V3 Double -> Vec3
fromCoordData = fmap round . (/ 3)

-- |Extracts all chains tagged with their IDs from a set of beads.
extractChains ::
     Maybe Int
  -- ^Square of the maximum chain segment length or Nothing if unbounded.
  -> (M.Map Serial Serial, M.Map Serial Serial)
  -- ^A bijection on a subset of the set of beads, representing connections between them.
  -> M.Map Serial (ChainId, DumpBeadInfo)
  -- ^The set of beads tagged with their PDB serial numbers.
  -> Either ReadError [(ChainId, [DumpBeadInfo])]
  -- ^The resulting list of chains or error if the input was invalid.
extractChains maxd (forMap, revMap) = go
  where
    go m | M.null m = Right []
         | otherwise = do
        let (serial, (chId, _)) = M.findMin m
        start <- findStart serial serial
        (chain, m') <- extractOneChain maxd forMap start chId m
        ((chId, chain) :) <$> go m'
    findStart start cur =
        case M.lookup cur revMap of
            Nothing -> pure cur
            Just next | next == start -> throwError $ "Atom " ++ show start ++ " lies on a connect cycle"
            Just next -> findStart start next

-- |Extracts one chain from a set of beads.
extractOneChain ::
     Maybe Int
  -- ^Square of the maximum chain segment length or Nothing if unbounded.
  -> M.Map Serial Serial
  -- ^A bijection between a subset of the set of beads, representing connections between them.
  -> Serial
  -- ^The PDB serial number of the beginning of the chain.
  -> ChainId
  -- ^The ID of the chain.
  -> M.Map Serial (ChainId, DumpBeadInfo)
  -- ^The set of beads tagged with their PDB serial numbers.
  -> Either ReadError ([DumpBeadInfo], M.Map Serial (ChainId, DumpBeadInfo))
  -- ^The resulting chain and the set of remaining beads or error if the input was invalid.
extractOneChain maxd connects start chId = go start
  where
    go cur m = do
        (chId', bead) <- findOrError ("Non-existing atom in connects: " ++ show cur) cur m
        when (chId /= chId')
            $ throwError $ "Atom " ++ show start ++ " from chain " ++ show chId ++ " is connected to atom "
                           ++ show cur ++ " from different chain " ++ show chId'
        let m' = M.delete cur m
        case M.lookup cur connects of
            Nothing -> pure ([bead], m')
            Just next -> do
                (nextBead : chain, m'') <- go next m'
                flip (maybe (pure ())) maxd $ \maxd' ->
                    when ((qd `on` dumpBeadPosition) bead nextBead > maxd')
                        $ throwError $ "Distance between connected atoms " ++ show cur ++ " and "
                                       ++ show next ++ " is greater than the square root of " ++ show maxd'
                pure (bead : nextBead : chain, m'')

mergeDumps :: [Dump] -> Either ReadError Dump
mergeDumps dumps = checkChains >> checkPositions >> pure (Dump allBinders allChains)
  where
    allBinders = concatMap dumpBinders dumps
    allChains = concatMap dumpChains dumps
    allAtomPositions = map dumpBeadPosition (concat allChains)
                    ++ map (^. position) allBinders
    connectedPositions = concatMap ((zip <*> tail) . map dumpBeadPosition) allChains

    checkPositions = foldM_ checkPosStep S.empty allAtomPositions
    checkPosStep s p = do
        when (S.member p s)
            $ throwError $ "Two atoms occupying the same position: " ++ show p
        pure $ S.insert p s

    checkChains = checkCrossConnects -- TODO: chain ids

    -- TODO: longer segments crossings
    checkCrossConnects = foldM_ checkCrossStep S.empty connectedPositions
    checkCrossStep s (v1, v2) | qd v1 v2 < 2 = pure s
    checkCrossStep s (v1, v2) = do
        let [u1, u2] = crossPoss v1 v2
        when (S.member (u1, u2) s || S.member (u2, u1) s)
            $ throwError $ "Chains cross on the segment" ++ show (v1, v2)
        pure $ S.insert (v1, v2) s

parseOrError :: Stream s Identity t => Parsec s () a -> s -> Either ReadError a
parseOrError p = left show . parse p ""

{-# ANN pdbChars "HLint: ignore Use String" #-}
-- |Characters used in string/character fields of PDB entries.
pdbChars :: [Char]
pdbChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
