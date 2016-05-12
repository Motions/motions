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
{-# LANGUAGE ViewPatterns #-}
module Bio.Motions.BED where

import Bio.Motions.Types
import Bio.Motions.Utils.Parsec

import Control.Monad
import Text.Parsec
import Text.Parsec.ByteString
import Foreign.Marshal.Utils(fromBool)
import qualified Data.Map as M

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
type NameMapping = M.Map String Int

-- |Combines BED files into EnergyVectors of beads
parseBEDs ::
     Int
  -- ^ Resolution of simulation
  -> [(String, Int)]
  -- ^ List of names of the chromosomes with their lenghts, sorted by their numbers
  -> [FilePath]
  -- ^ Locations of BED files
  -> IO [[EnergyVector]]
parseBEDs resolution (unzip -> (names, lengths)) fileNames = do
    let namesToNums = M.fromList $ zip names [0..]
    parses <- zipWithM (parseFromFile . parseBED namesToNums) [0..] fileNames
    beds <- concat <$> mapM (either (ioError . userError . show) return) parses
    let newLengths = map (`divCeil` resolution) lengths
    let bsInfos = map (applyResolution resolution) beds
    return $ collect (length fileNames) newLengths bsInfos

-- |Parses a single BED file
parseBED :: NameMapping -> Int -> Parser [BindingSiteInfo]
parseBED mapping bsType = do
    optional $ string "Track" >> manyTill anyChar endOfLine
    endBy (line mapping bsType) endOfLine

-- |Parses one line of BED
line :: NameMapping -> Int -> Parser BindingSiteInfo
line mapping bsType = do
    bsChain <- chromosome mapping
    void tab
    bsFrom <- int
    void tab
    bsTo <- int
    unless (bsFrom <= bsTo) $ fail "Binding site ends before it begins"
    optional (tab >> manyTill anyChar (lookAhead endOfLine))
    return BindingSiteInfo{..}

-- |Returns the number of the chain with the given name
-- or fails if there is no chain with that name
getChainNumber :: NameMapping -> String -> Parser Int
getChainNumber mapping name = do
    unless (name `M.member` mapping) $ fail $ "Unrecognised chromosome: " ++ name
    return $ mapping M.! name

-- |Parses BED 'chromosome' column
chromosome :: NameMapping -> Parser Int
chromosome mapping = word >>= getChainNumber mapping

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
