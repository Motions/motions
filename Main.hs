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
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Engine
import Bio.Motions.StateInitialisation

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad
import qualified Data.Vector.Unboxed as U
import Options.Applicative
import Data.Proxy
import Data.Maybe
import Data.List

data InitialisationSettings = InitialisationSettings
    { bedFiles :: [FilePath]
    , chainLengthsFile :: FilePath
    , bindersCountsFile :: FilePath
    , radius :: Int
    , resolution :: Int
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
    , verboseCallbacks :: Bool
    , freezeFile :: Maybe FilePath
    }

mkRunSettings :: RunSettings' -> RunSettings repr score
mkRunSettings RunSettings'{..} = RunSettings{..}

type Run' gen = RunSettings' -> Dump -> (RandT gen IO) Dump
type Run gen score = Proxy score -> Run' gen

newtype RunRepr = RunRepr { runRepr :: forall gen score. (RandomGen gen, Score score) => Run gen score }
newtype RunScore = RunScore { runScore :: forall gen. RandomGen gen =>
                                (forall score. Score score => Run gen score) -> Run' gen }

reprMap :: [(String, RunRepr)]
reprMap = [ ("PureChain", runPureChain)
          , ("IOChain", runIOChain)
          ]
  where
    runPureChain = RunRepr $ \(_ :: _ score) rs ->
        simulate (mkRunSettings rs :: RunSettings PureChainRepresentation score)
    runIOChain = RunRepr $ \(_ :: _ score) rs ->
        simulate (mkRunSettings rs :: RunSettings IOChainRepresentation score)

scoreMap :: [(String, RunScore)]
scoreMap = [ ("StandardScore", runStandardScore)
           , ("TestCb", runTestCb)
           ]
  where
    runStandardScore = RunScore $ \run -> run (Proxy :: Proxy StandardScore)
    runTestCb = RunScore $ \run -> run (Proxy :: Proxy TestCb)

loadInts :: FilePath -> IO [Int]
loadInts path = withFile path ReadMode $ fmap (map read . words) . hGetLine

load :: (MonadIO m, MonadRandom m) => InitialisationSettings -> m Dump
load InitialisationSettings{..} = do
    chainLengths <- liftIO $ loadInts chainLengthsFile
    energyVectors <- liftIO $ parseBEDs resolution chainLengths bedFiles
    bindersCounts <- liftIO $ loadInts bindersCountsFile
    let evLength = U.length . getEnergyVector . head . head $ energyVectors
    when (evLength /= length bindersCounts + 1) $ error binderErrorMsg
    maybeDump <- initialise initAttempts radius bindersCounts energyVectors
    case maybeDump of
      Nothing -> error "Failed to initialise"
      Just dump -> pure dump
  where
    binderErrorMsg = "The number of different binder types must be the same as the number of chain features \
                      \ (BED files) minus one (the lamin feature)."

run :: InitialisationSettings -> SimulationSettings -> IO ()
run initialiseSettings SimulationSettings{..} = do
    gen <- newStdGen
    flip evalRandT gen $ do
        dump <- load initialiseSettings
        simulate runSettings dump
    -- TODO: do something with the dump?
    pure ()
  where
    simulate = runScore runRepr
    RunScore runScore = fromMaybe (error "Invalid score") $ lookup scoreName scoreMap
    RunRepr runRepr = fromMaybe (error "Invalid representation") $ lookup reprName reprMap

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
    <*> strOption
        (long "bindersfile"
        <> short 'b'
        <> metavar "BINDERS-COUNTS-FILE"
        <> help "File containing counts of binders of different types (excluding lamin) \
                 \ - integers separated with spaces")
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
    <*> switch
        (long "verbose-callbacks"
        <> short 'v'
        <> help "Output callback results in verbose format")
    <*> optional (strOption
        (long "freezefile"
        <> metavar "FREEZE_FILE"
        <> help "File containing the ranges of frozen beads' indices"))

simulationParser :: Parser SimulationSettings
simulationParser = SimulationSettings
    <$> runSettingsParser
    <*> strOption
        (long "representation"
        <> value defaultRepr
        <> showDefault
        <> help ("The representation used through the simulation, one of the following: " ++ reprs))
    <*> strOption
        (long "score"
        <> value defaultScore
        <> showDefault
        <> help ("The score function used through the simulation, one of the following: " ++ scores))
  where
    defaultRepr = fst . head $ reprMap
    defaultScore = fst . head $ scoreMap
    reprs = showKeys reprMap
    scores = showKeys scoreMap
    showKeys = intercalate ", " . map fst

parser :: Parser (InitialisationSettings, SimulationSettings)
parser = (,) <$> initialiseParser <*> simulationParser
