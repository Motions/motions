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

import qualified Control.Applicative as A
import Control.Monad.State
--import Control.Monad.Random hiding (fromList)
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Maybe
import Text.Parsec
import Text.Parsec.ByteString
import Foreign.Marshal.Utils(fromBool)
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U

import Text.Read
import GHC.Exts (fromList)

-- |Represents a binding site
data BindingSiteInfo = BindingSiteInfo
    { bsChain :: Int
    , bsFrom :: Int
    , bsTo :: Int
    , bsType :: Int
    }
    deriving (Eq, Show)

-- |Combines BED files into EnergyVectors of beads
parseBEDs ::
     Int
  -- ^ Resolution of simulation
  -> [Int]
  -- ^ Lenghts of chromosomes (number of base pairs)
  -> [FilePath]
  -- ^ Locations of BED files
  -> IO [[EnergyVector]]
parseBEDs resolution lengths fileNames = do
  parses <- zipWithM (parseFromFile . parseBED) [0..] fileNames
  beds <- concat <$> mapM (either (ioError . userError . show) return) parses
  let newLengths = map (`divCeil` resolution) lengths
  let bsInfos = map (applyResolution resolution) beds
  return $ collect (length fileNames) newLengths bsInfos

-- |Parses a single BED file
parseBED :: Int -> Parser [BindingSiteInfo]
parseBED bsType = do
  optional $ string "Track" >> manyTill anyChar endOfLine
  endBy (line bsType) endOfLine

-- |Parses one line of BED
line :: Int -> Parser BindingSiteInfo
line bsType = do
  bsChain <- chromosome
  tab
  bsFrom <- int
  tab
  bsTo <- int
  unless (bsFrom <= bsTo) $ fail "Binding site ends before it begins"
  optional (tab >> manyTill anyChar (lookAhead endOfLine))
  return BindingSiteInfo{..}

-- |Parses BED 'chromosome' column
chromosome :: Parser Int
chromosome = optional (string "chr") >> (\x -> x-1) A.<$> int

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
