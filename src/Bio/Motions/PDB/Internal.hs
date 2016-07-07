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
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS

type Serial = Int
type ReadError = String

data FrameHeader =
    StepHeader { headerSeqNum :: FrameCounter
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
    , chainName :: M.Map ChainId String
    -- ^ Maps chain numbers to their full names
    , binderTypeName :: M.Map BinderType String
    -- ^ Maps binder types to their full names
    }

data RevPDBMeta = RevPDBMeta
    { revBeadRes :: M.Map String EnergyVector
    , revBinderRes :: M.Map String BinderType
    , revChainId :: M.Map Char ChainId
    , revChainName :: M.Map String ChainId
    , revBinderTypeName :: M.Map String BinderType
    }
    deriving (Eq, Show)

data PDBEntry = PDBHeader  { classification :: !String }
              | PDBTitle   { title :: !String }
              | PDBAtom    { serial :: !Serial    -- ^ Atom serial number
                           , name :: !String      -- ^ Atom name
                           , resName :: !String   -- ^ Residue name
                           , chainID :: !Char     -- ^ Chain identifier
                           , resSeq :: !Int       -- ^ Residue sequence number
                           , coords :: !Vec3      -- ^ Coordinates (X, Y, Z) in Angstroms
                           } -- The other ATOM fields (occupancy, tempFactor etc.) are always 0 or empty.
              | PDBConnect { fstSerial :: !Serial
                           , sndSerial :: !Serial
                           }
    deriving Show

data PDBMetaEntry = EnergyVectorMap EnergyVector String
                  | BinderTypeMap BinderType String
                  | ChainIdMap ChainId Char
                  | ChainNameMap ChainId String
                  | BinderTypeNameMap BinderType String
    deriving (Eq, Show)

printPDBMetaEntry :: PDBMetaEntry -> String
printPDBMetaEntry (EnergyVectorMap ev str) = "EV " ++ (show . toList) ev ++ " " ++ str
printPDBMetaEntry (BinderTypeMap bt str) = "BT " ++ (show . getBinderType) bt ++ " " ++ str
printPDBMetaEntry (ChainIdMap ch c) = "CH " ++ show ch ++ " " ++ [c]
printPDBMetaEntry (ChainNameMap ch name) = "NMCH " ++ show ch ++ " " ++ name
printPDBMetaEntry (BinderTypeNameMap (BinderType bt) name) = "NMBT " ++ show bt ++ " " ++ name

parsePDBMetaData :: BS.ByteString -> Either ReadError [PDBMetaEntry]
parsePDBMetaData = parseOrError metaEntries
  where
    metaEntries :: Parser [PDBMetaEntry]
    metaEntries = sepEndBy metaEntry endOfLine <* eof

    metaEntry :: Parser PDBMetaEntry
    metaEntry = energyVecMap <|> binderTypeMap <|> chainIdMap <|> chainNameMap <|> binderTypesMap

    energyVecMap :: Parser PDBMetaEntry
    energyVecMap = EnergyVectorMap <$> (try (string "EV") *> spaces *> energyVector <* spaces)
                                   <*> energyVectorString
                                   <?> "Energy vector mapping"

    binderTypeMap :: Parser PDBMetaEntry
    binderTypeMap = BinderTypeMap <$> (try (string "BT") *> spaces *> binderType <* spaces)
                                  <*> binderTypeString
                                  <?> "Binder type mapping"

    chainIdMap :: Parser PDBMetaEntry
    chainIdMap = ChainIdMap <$> (try (string "CH") *> spaces *> int <* spaces)
                            <*> oneOf pdbChars
                            <?> "Chain id mapping"

    binderTypesMap :: Parser PDBMetaEntry
    binderTypesMap = BinderTypeNameMap <$> (try (string "NMBT") *> spaces *> binderType <* spaces)
                                       <*> word
                                       <?> "Binder type name mapping"

    chainNameMap :: Parser PDBMetaEntry
    chainNameMap = ChainNameMap <$> (try (string "NMCH") *> spaces *> int <* spaces)
                                 <*> word
                                 <?> "Chain name mapping"

    binderTypeString :: Parser String
    binderTypeString = replicateM 3 (oneOf pdbChars)

    binderType :: Parser BinderType
    binderType = BinderType <$> int

    energyVectorString :: Parser String
    energyVectorString = replicateM 3 (oneOf pdbChars)

toPDBMetaData :: PDBMeta -> [PDBMetaEntry]
toPDBMetaData PDBMeta{..} = map (uncurry EnergyVectorMap) (toList beadRes)
                         ++ map (uncurry BinderTypeMap) (toList binderRes)
                         ++ map (uncurry ChainIdMap) (toList chainId)
                         ++ map (uncurry ChainNameMap) (toList chainName)
                         ++ map (uncurry BinderTypeNameMap) (toList binderTypeName)

toRevPDBMeta :: [PDBMetaEntry] -> Either ReadError RevPDBMeta
toRevPDBMeta = foldM step (RevPDBMeta M.empty M.empty M.empty M.empty M.empty) >=> \meta ->
    validate meta >> pure meta
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
    step meta (ChainNameMap ch name)
      | M.member name (revChainName meta) = throwError $ "Duplicate chain names: " ++ name
      | otherwise = pure meta { revChainName = M.insert name ch $ revChainName meta }
    step meta (BinderTypeNameMap bt name)
      | M.member name (revBinderTypeName meta) = throwError $ "Duplicate binder names: " ++ name
      | otherwise = pure meta { revBinderTypeName = M.insert name bt $ revBinderTypeName meta}

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
        unless (sort chs == sort (M.elems revChainName))
            $ throwError "Every chain needs to have a name."
        unless (sort bts == sort (map getBinderType . M.elems $ revBinderTypeName)) $
            throwError "Every binder type needs to have a name"

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

toCoordData :: Vec3 -> Vec3
toCoordData = (* 3)
{-# INLINE toCoordData #-}

toConnectData :: Serial -> PDBEntry
toConnectData i = PDBConnect i (i + 1)

-- |Converts a list of PDB entries to a dump.
fromPDBData ::
      RevPDBMeta
   -- ^The mappings used to convert from the PDB format.
   -> Maybe Int
   -- ^Square of the maximum chain segment length or Nothing if unbounded.
   -> [PDBEntry]
   -- ^The list of PDB entries.
   -> Either ReadError Dump
   -- ^The resulting dump or error if the input was invalid.
fromPDBData meta maxd es = do
    dumpBinders <- mapM (fromBinderData meta) binderEntries
    connects <- fromConnectsData connectEntries
    beadMap <- M.fromList <$> mapM (\e -> (serial e, ) <$> fromBeadData meta e) beadEntries
    (chainIds, dumpChains) <- unzip . sortWith fst <$> extractChains maxd connects beadMap
    onDuplicates chainIds $ \ch ->
        throwError $ "Two chains (not connected) with the same chain id: " ++ show ch
    unless (and $ zipWith (==) chainIds [0..]) $
        throwError "Chain numbers are not consecutive natural numbers starting from 0"
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

fromCoordData :: Vec3 -> Vec3
fromCoordData = fmap (`div` 3)
{-# INLINE fromCoordData #-}

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
                forM_ maxd $ \maxd' ->
                    when ((qd `on` dumpBeadPosition) bead nextBead > maxd')
                        $ throwError $ "Distance between connected atoms " ++ show cur ++ " and "
                                       ++ show next ++ " is greater than the square root of " ++ show maxd'
                pure (bead : nextBead : chain, m'')

mergeDumps :: [Dump] -> Either ReadError Dump
mergeDumps dumps = checkPositions >> checkChains >> pure mergedDump
  where
    allBinders = concatMap dumpBinders dumps
    allChains = concatMap dumpChains dumps
    mergedDump = Dump allBinders allChains

    allAtomPositions = map dumpBeadPosition (concat allChains)
                    ++ map (^. position) allBinders
    connectedPositions = concatMap ((zip <*> tail) . map dumpBeadPosition) allChains

    checkPositions = foldM_ checkPosStep S.empty allAtomPositions
    checkPosStep s p = do
        when (S.member p s)
            $ throwError $ "Two atoms occupying the same position: " ++ show p
        pure $ S.insert p s

    checkChains = checkCrossConnects

    checkCrossConnects = mapM_ checkCrossStep connectedPositions
    checkCrossStep (v1, v2) = when (intersects v1 v2)
        $ throwError $ "Chains cross on the segment" ++ show (v1, v2)

    intersects
      | maxQd <= 2 = sqrt2IntersectsChain beadSpace
      | otherwise  = intersectsChain maxQd beadSpace

    beadSpace = foldr (\b -> HM.insert (b ^. position) (asAtom b)) HM.empty allBeads
    allBeads = concat . dumpIndexedChains $ mergedDump
    maxQd = foldr (max . uncurry qd) 0 connectedPositions

parseOrError :: Stream s Identity t => Parsec s () a -> s -> Either ReadError a
parseOrError p = left show . parse p ""

{-# ANN pdbChars "HLint: ignore Use String" #-}
-- |Characters used in string/character fields of PDB entries.
pdbChars :: [Char]
pdbChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
