{- |
Module      : Main
Description : Contains the main function.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import Bio.Motions.EnabledCallbacks
import Bio.Motions.Types
import Bio.Motions.BED
import Bio.Motions.Representation.Chain
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Engine

import System.IO
import Control.Monad.IO.Class
--import Control.Monad.State
--import Control.Monad.Trans.Maybe
import Control.Monad.Random
import qualified Data.Vector.Unboxed as U
import Options.Applicative
import Data.Proxy
import Data.Maybe

initialize :: MonadRandom m => Int -> Int -> Int -> Int -> [[EnergyVector]] -> m (Maybe Dump)
initialize = undefined

data InitializeSettings = InitializeSettings
    { bedFiles :: [FilePath]
    , chainLengthsFile :: FilePath
    , radius :: Int
    , resolution :: Int
    , binderCount :: Int
    , initAttempts :: Int
    }

data SimulationSettings = SimulationSettings
    { runSettings :: RunSettings'
    , reprName :: String
    , scoreName :: String
    }

data RunSettings' = RunSettings'
    { pdbFile :: FilePath
    , numSteps :: Int
    , writeIntermediatePDB :: Bool
    }

mkRunSettings :: RunSettings' -> RunSettings repr score
mkRunSettings RunSettings'{..} = RunSettings{..}

type Run' gen = RunSettings' -> Dump -> (RandT gen IO) Dump
type Run gen score = Proxy score -> Run' gen

reprMap :: RandomGen gen => [(String, forall score. Score score => Run gen score)]
reprMap = [ ("PureChain", runPureChain)
          , ("IOChain", runIOChain)
          ]
  where
    runPureChain :: (RandomGen gen, Score score) => Run gen score
    runPureChain (_ :: _ score) rs = simulate (mkRunSettings rs :: RunSettings PureChainRepresentation score)

    runIOChain :: (RandomGen gen, Score score) => Run gen score
    runIOChain (_ :: _ score) rs = simulate (mkRunSettings rs :: RunSettings IOChainRepresentation score)

scoreMap :: RandomGen gen => [(String, (forall score. Score score => Run gen score) -> Run' gen)]
scoreMap = [ ("StandardScore", runStandardScore)
           , ("TestCb", runTestCb)
           ]
  where
    runStandardScore :: Run gen StandardScore -> Run' gen
    runStandardScore run = run (Proxy :: Proxy StandardScore)

    runTestCb :: Run gen TestCb -> Run' gen
    runTestCb run = run (Proxy :: Proxy TestCb)

loadChainLengths :: FilePath -> IO [Int]
loadChainLengths path = undefined

load :: (MonadIO m, MonadRandom m) => InitializeSettings -> m Dump
load InitializeSettings{..} = do
    chainLengths <- liftIO $ loadChainLengths chainLengthsFile
    energyVectors <- liftIO $ parseBEDs resolution chainLengths bedFiles
    let resRad = radius `div` resolution
        binderTypeCount = U.length . getEnergyVector . head . head $ energyVectors
    maybeDump <- initialize initAttempts resRad binderCount binderTypeCount energyVectors
    case maybeDump of
      Nothing -> error "Failed to initialize"
      Just dump -> pure dump

run :: InitializeSettings -> SimulationSettings -> IO ()
run initializeSettings SimulationSettings{..} = do
    gen <- newStdGen
    (dump, gen') <- flip runRandT gen $ load initializeSettings
    dump' <- flip evalRandT gen $ run runSettings dump
    -- TODO: do something with the dump?
    pure ()
  where
    run = runScore runRepr
    runScore = fromMaybe (error "Invalid score") $ lookup scoreName scoreMap
    runRepr = fromMaybe (error "Invalid representation") $ lookup reprName reprMap

main :: IO ()
main = uncurry run =<< execParser
    (info (helper <*> parser)
          (fullDesc <> progDesc "Perform a MCMC simulation of chromatine movements"))

initializeParser :: Parser InitializeSettings
initializeParser = InitializeSettings
    <$> many (strArgument $ metavar "BED files...")
    <*> strOption
        (long "lengthsfile"
        <> short 'l'
        <> metavar "CHAIN-LENGTHS-FILE"
        <> help "File containing chain lengths - integers separated with spaces")
    <*> option auto
        (long "radius"
        <> short 'r'
        <> metavar "RADIUS"
        <> help "Radius of the bounding sphere after applying resolution")
    <*> option auto
        (long "resolution"
        <> short 'x'
        <> metavar "RESOLUTION"
        <> help "Simulation resolution - chain molecules per bead")
    <*> option auto
        (long "num-binders"
        <> short 'b'
        <> metavar "BINDER-COUNT"
        <> help "Number of binders")
    <*> option auto
        (long "num-attempts"
        <> short 'n'
        <> metavar "INIT-ATTEMPTS"
        <> help "Number of state initialization attempts")

runSettingsParser :: Parser RunSettings'
runSettingsParser = RunSettings'
    <$> strOption
        (long "outputfile"
        <> short 'o'
        <> metavar "PDB-OUTPUT-FILE")
    <*> option auto
        (long "steps"
        <> short 's'
        <> metavar "NUM-STEPS"
        <> help "Number of simulation steps")
    <*> switch
        (long "intermediate-states"
        <> short 'i'
        <> help "Write intermediate states to PDB file")

simulationParser :: Parser SimulationSettings
simulationParser = SimulationSettings
    <$> runSettingsParser
    <*> strOption
        (long "representation")
    <*> strOption
        (long "score")

parser :: Parser (InitializeSettings, SimulationSettings)
parser = (,) <$> initializeParser <*> simulationParser
