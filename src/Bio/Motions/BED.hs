{- |
Module      : Bio.Motions.BED
Description : Parsing BED into chain of beeds
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
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

parseBEDs :: Int -> Int -> [Int] -> [String] -> IO [[EnergyVector]]
parseBEDs resolution typesCount lengths fileNames = do
  parses <- mapM (\(nr, bed) -> parseFromFile (parseBED nr) bed) (zip [0..] fileNames)
  beds <- concat <$> mapM (either (ioError . userError . show) return) parses
  let newLengths = map (`divCeil` resolution) lengths
  let bsInfos = map (applyResolution resolution) beds
  return $ collect typesCount newLengths bsInfos

parseBED :: Int -> Parser [BindingSiteInfo]
parseBED bsType = do
  optional $ string "Track" >> manyTill anyChar eol
  endBy (line bsType) eol

eol :: Parser String
eol = try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\r"
     <|> string "\n"
     <?> "End of line"

line :: Int -> Parser BindingSiteInfo
line bsType = do
  bsChain <- chromosome
  char '\t'
  bsFrom <- parseInt
  char '\t'
  bsTo <- parseInt
  unless (bsFrom <= bsTo) $ fail "Binding site ends before it begins"
  optional (char '\t' >> manyTill anyChar (lookAhead eol))
  return BindingSiteInfo{..}

parseInt :: Parser Int
parseInt = read A.<$> many digit

chromosome :: Parser Int
chromosome = optional (string "chr") >> (\x -> x-1) A.<$> parseInt

applyResolution :: Int -> BindingSiteInfo -> BindingSiteInfo
applyResolution resolution x@BindingSiteInfo{..} =
  x {bsFrom = bsFrom `div` resolution, bsTo = bsTo `divCeil` resolution}

divCeil :: (Integral a) => a -> a -> a
divCeil x y = fromBool (x `mod` y > 0) + div x y

collect :: Int -> [Int] -> [BindingSiteInfo] -> [[EnergyVector]]
collect typesCount lengths bsInfos = map (map (EnergyVector . U.fromList)) valuesPoses
  where
    expandBsInfos = concat [ zip3 (repeat bsChain) [bsFrom .. bsTo] (repeat bsType) | BindingSiteInfo{..} <- bsInfos]
    dict = foldl incrementMap M.empty expandBsInfos
    incrementMap m new = M.alter (\x -> Just $ 1 + fromMaybe 0 x) new m
    allPos = [[[(chr, pos, bsType) | bsType <- [0 .. (typesCount - 1)]] | pos <- [0 .. (chrLen -1)]]
      | (chr, chrLen) <- zip [0..] lengths]
    valuesPoses = map (map (map (\x -> M.findWithDefault 0 x dict))) allPos








