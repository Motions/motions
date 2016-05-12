{- |
Module      : Bio.Motions.BED
Description : Parsing BED into chain of beads
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
module Bio.Motions.BED where

import Bio.Motions.Types
import Bio.Motions.Utils.Parsec

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Parsec
import Foreign.Marshal.Utils(fromBool)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Bimap as BM
import Control.Error.Util

import Debug.Trace

import GHC.Exts (fromList)

-- |Represents a binding site
data BindingSiteInfo = BindingSiteInfo
    { bsChain :: Int
    , bsFrom :: Int
    , bsTo :: Int
    , bsType :: Int
    }
    deriving (Eq, Show)

-- |Translation between chromosome names and chain numbers.
type NameMapping = BM.Bimap String Int

-- |Parser enhanced in mapping between chromosome name and their numbers
type ChrParser a = Parsec String NameMapping a

parseChrFromFile :: Int -> String -> StateT NameMapping (ExceptT ParseError IO) [BindingSiteInfo]
parseChrFromFile bsType fileName = StateT $
    \bm -> ExceptT $ runParser (parseBED bsType) bm fileName <$> readFile fileName

-- |Combines BED files into EnergyVectors of beads
parseBEDs ::
     Int
  -- ^ Resolution of simulation
  -> [(String, Int)]
  -- ^ Lenghts of chromosomes (number of base pairs)
  -> [FilePath]
  -- ^ Locations of BED files
  -> IO ([[EnergyVector]], [String])
parseBEDs resolution chains fileNames = do
    let lengths = M.fromList chains
    let funcs = zipWith parseChrFromFile [0..] fileNames
    let namesToNums = BM.fromList [(name, num) | ((name, _), num) <- zip chains [0..]]
    traceM $ show namesToNums
    (parses, mapping) <- exceptT (ioError . userError . show) return $ runStateT (sequence funcs) namesToNums
    let names = L.nub $ map snd (BM.toAscListR mapping) ++ M.keys lengths
    let lengthsList = map (lengths M.!) names
    let beds = concat parses
    let newLengths = map (`divCeil` resolution) lengthsList
    let bsInfos = map (applyResolution resolution) beds
    return (collect (length fileNames) newLengths bsInfos, map fst chains)

-- |Parses a single BED file
parseBED :: Int -> ChrParser ([BindingSiteInfo], NameMapping)
parseBED bsType = do
    optional $ string "Track" >> manyTill anyChar endOfLine
    chains <- endBy (line bsType) endOfLine
    mapping <- getState
    return (chains, mapping)

-- |Parses one line of BED
line :: Int -> ChrParser BindingSiteInfo
line bsType = do
    bsChain <- chromosome
    void tab
    bsFrom <- int
    void tab
    bsTo <- int
    unless (bsFrom <= bsTo) $ fail "Binding site ends before it begins"
    optional (tab >> manyTill anyChar (lookAhead endOfLine))
    return BindingSiteInfo{..}

-- |Returns the number of the chain with the given name.
-- |If the name hasn't been used yet, it assignes
-- |the smallest non used positive number to it
getChainNumber :: String -> ChrParser Int
getChainNumber name = do
    mapping <- getState
    traceM $ show mapping
    unless (name `BM.member` mapping) $ fail $ "Unrecognised chromosome: " ++ name
    return $ mapping BM.! name

-- |Parses BED 'chromosome' column
chromosome :: ChrParser Int
chromosome = word >>= getChainNumber

-- |Groups nucleotides according to the resolution parameter
applyResolution :: Int -> BindingSiteInfo -> BindingSiteInfo
applyResolution resolution x@BindingSiteInfo{..} =
    x {bsFrom = bsFrom `div` resolution, bsTo = bsTo `div` resolution}

-- |Ceil of a quotient
divCeil :: (Integral a) => a -> a -> a
divCeil x y = fromBool (x `mod` y > 0) + div x y

-- |Gathers information about all binding sites and returns a EnergyVector for each bead
-- in the simulation
collect :: Int -> [Int] -> [BindingSiteInfo] -> [[EnergyVector]]
collect typesCount lengths bsInfos =
    [[fromList [M.findWithDefault 0 (chr, pos, bsType) dict
      | bsType <- [0..typesCount - 1]] | pos <- [0..chrLen - 1]] | (chr, chrLen) <- zip [0..] lengths]
  where
    expandBsInfos = concat [map (bsChain,,bsType) [bsFrom..bsTo] | BindingSiteInfo{..} <- bsInfos]
    dict = foldl incrementMap M.empty expandBsInfos
    incrementMap m new = M.alter (Just . maybe 1 (+1)) new m
