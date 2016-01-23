{- |
Module      : Bio.Motions.BED
Description : Parsing BED into chain of beads
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Bio.Motions.BED where

import Bio.Motions.Types

import qualified Control.Applicative as A
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Maybe
import Text.ParserCombinators.Parsec
import Foreign.Marshal.Utils
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U

import Text.Read

-- |Represents a binding site
data BindingSiteInfo = BindingSiteInfo
    { bsChain :: Int
    , bsFrom :: Int
    , bsTo :: Int
    , bsType :: Int
    }
    deriving (Eq, Show)
-- |Receives desired resolution, lengths of all chromosomes (bp) and list of BED
-- files, that contain information about binding sites and return energy vector
-- for each bead in the simulation
parseBEDs :: Int -> [Int] -> [String] -> IO [[EnergyVector]]
parseBEDs resolution lengths fileNames = do
  parses <- mapM (\(nr, bed) -> parseFromFile (parseBED nr) bed) (zip [0..] fileNames)
  beds <- concat <$> mapM (either (ioError . userError . show) return) parses
  let newLengths = map (`divCeil` resolution) lengths
  let bsInfos = map (applyResolution resolution) beds
  return $ collect (length fileNames) newLengths bsInfos

-- |Parses a single BED file
parseBED :: Int -> Parser [BindingSiteInfo]
parseBED bsType = do
  optional $ string "Track" >> manyTill anyChar eol
  endBy (line bsType) eol

-- | Reads end of line
eol :: Parser String
eol = try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\r"
     <|> string "\n"
     <?> "End of line"

-- | Parses one line of BED
line :: Int -> Parser BindingSiteInfo
line bsType = do
  bsChain <- chromosome
  tab
  bsFrom <- parseInt
  tab
  bsTo <- parseInt
  unless (bsFrom <= bsTo) $ fail "Binding site ends before it begins"
  optional (tab >> manyTill anyChar (lookAhead eol))
  return BindingSiteInfo{..}

parseInt :: Parser Int
parseInt = (read A.<$> many1 digit) <?> "Positive Integer"

-- |Parses BED 'chromosome' column
chromosome :: Parser Int
chromosome = optional (string "chr") >> (\x -> x-1) A.<$> parseInt

-- |Groups nucleotides according to the resolution parameter
applyResolution :: Int -> BindingSiteInfo -> BindingSiteInfo
applyResolution resolution x@BindingSiteInfo{..} =
  x {bsFrom = bsFrom `div` resolution, bsTo = bsTo `div` resolution}

-- |Ceil of a division
divCeil :: (Integral a) => a -> a -> a
divCeil x y = fromBool (x `mod` y > 0) + div x y

-- |Gathers information about all binding sites and return a EnergyVector for each bead
-- in the simulation
collect :: Int -> [Int] -> [BindingSiteInfo] -> [[EnergyVector]]
collect typesCount lengths bsInfos = valuesPoses
  where
    expandBsInfos = concat [map (bsChain,,bsType) [bsFrom .. bsTo] | BindingSiteInfo{..} <- bsInfos]
    dict = foldl incrementMap M.empty expandBsInfos
    incrementMap m new = M.alter (Just . maybe 1 (+1)) new m
    valuesPoses = [[EnergyVector . U.fromList $ [M.findWithDefault 0 (chr, pos, bsType) dict
      | bsType <- [0..typesCount - 1]] | pos <- [0..chrLen - 1]] | (chr, chrLen) <- zip [0..] lengths]
