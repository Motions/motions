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
import Bio.Motions.StateInitialisation

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Random
import qualified Data.Vector.Unboxed as U
import Options.Applicative
import Data.Proxy
import Data.Maybe

data InitialisationSettings = InitialisationSettings
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

newtype RunRepr gen = RunRepr { runRepr :: forall score. Score score => Run gen score }
newtype RunScore gen = RunScore { runScore :: (forall score. Score score => Run gen score) -> Run' gen }

reprMap :: RandomGen gen => [(String, RunRepr gen)]
reprMap = [ ("PureChain", runPureChain)
          , ("IOChain", runIOChain)
          ]
  where
    runPureChain = RunRepr $ \(_ :: _ score) rs ->
        simulate (mkRunSettings rs :: RunSettings PureChainRepresentation score)
    runIOChain = RunRepr $ \(_ :: _ score) rs ->
        simulate (mkRunSettings rs :: RunSettings IOChainRepresentation score)

scoreMap :: RandomGen gen => [(String, RunScore gen)]
scoreMap = [ ("StandardScore", runStandardScore)
           , ("TestCb", runTestCb)
           ]
  where
    runStandardScore = RunScore $ \run -> run (Proxy :: Proxy StandardScore)
    runTestCb = RunScore $ \run -> run (Proxy :: Proxy TestCb)

loadChainLengths :: FilePath -> IO [Int]
loadChainLengths path = withFile path ReadMode $ fmap (map read . words) . hGetLine

load :: (MonadIO m, MonadRandom m) => InitialisationSettings -> m Dump
load InitialisationSettings{..} = do
    chainLengths <- liftIO $ loadChainLengths chainLengthsFile
    energyVectors <- liftIO $ parseBEDs resolution chainLengths bedFiles
    let resRad = radius `div` resolution
        binderTypeCount = flip (-) 1 . U.length . getEnergyVector . head . head $ energyVectors
    maybeDump <- initialise initAttempts resRad binderCount binderTypeCount energyVectors
    case maybeDump of
      Nothing -> error "Failed to initialise"
      Just dump -> pure dump

run :: InitialisationSettings -> SimulationSettings -> IO ()
run initialiseSettings SimulationSettings{..} = do
    gen <- newStdGen
    (dump, gen') <- flip runRandT gen $ load initialiseSettings
    dump' <- flip evalRandT gen $ run runSettings dump
    -- TODO: do something with the dump?
    pure ()
  where
    run = runScore runRepr
    (RunScore runScore) = fromMaybe (error "Invalid score") $ lookup scoreName scoreMap
    (RunRepr runRepr) = fromMaybe (error "Invalid representation") $ lookup reprName reprMap

main :: IO ()
main = uncurry run =<< execParser
    (info (helper <*> parser)
          (fullDesc <> progDesc "Perform a MCMC simulation of chromatin movements"))

initialiseParser :: Parser InitialisationSettings
initialiseParser = InitialisationSettings
    <$> some (strArgument $ metavar "BED files...")
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

parser :: Parser (InitialisationSettings, SimulationSettings)
parser = (,) <$> initialiseParser <*> simulationParser
