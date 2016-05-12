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
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Either
import Text.Parsec
import Text.Parsec.ByteString
import Foreign.Marshal.Utils(fromBool)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Bimap as BM

import GHC.Exts (fromList)

-- |Represents a binding site
data BindingSiteInfo = BindingSiteInfo
    { bsChain :: Int
    , bsFrom :: Int
    , bsTo :: Int
    , bsType :: Int
    }
    deriving (Eq, Show)

-- |Translation between chromosome names, and chain numbers
-- |the translation is non cannonical, names will have assigned
-- |numbers in order of apperance
type NameMapping = BM.Bimap String Int

-- |Parser enhanced in mapping between chromosome nameand their numbers
type ChrParser a = Parsec String NameMapping a

parseChrFromFile :: Int -> String -> StateT NameMapping (EitherT ParseError IO) [BindingSiteInfo]
parseChrFromFile bsType fileName = StateT (\bm -> EitherT $ runParser (parseBED bsType) bm fileName <$> readFile fileName)

-- |Combines BED files into EnergyVectors of beads
parseBEDs ::
     Int
  -- ^ Resolution of simulation
  -> M.Map String Int
  -- ^ Lenghts of chromosomes (number of base pairs)
  -> [FilePath]
  -- ^ Locations of BED files
  -> IO ([[EnergyVector]], [String])
parseBEDs resolution lengths fileNames = do
    let funcs = zipWith parseChrFromFile [0..] fileNames
    (parses, mapping) <- eitherT (ioError . userError . show) return $ runStateT (sequence funcs) BM.empty
    let names = L.nub $ map snd (BM.toAscListR mapping) ++ M.keys lengths
    let lengthsList = map (lengths M.!) names
    let beds = concat parses
    let newLengths = map (`divCeil` resolution) lengthsList
    let bsInfos = map (applyResolution resolution) beds
    return (collect (length fileNames) newLengths bsInfos, names)

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
maybeAssignAndGetNumber :: String -> ChrParser Int
maybeAssignAndGetNumber name = do
    map <- getState
    unless (name `BM.member` map) $ do
        let (num, _) = if not $ BM.null map then BM.findMaxR map else (-1, "nothing")
        modifyState (BM.insert name (num + 1))
    map <- getState
    return $ map BM.! name

-- |Parses BED 'chromosome' column
chromosome :: ChrParser Int
chromosome = word >>= maybeAssignAndGetNumber

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
