{-# LANGUAGE RecordWildCards #-}
module Main where

import Bio.Motions.Types
import Bio.Motions.BED
import Bio.Motions.Representation.Dump
import Bio.Motions.Engine

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Random
import qualified Data.Vector.Unboxed as U
import Options.Applicative

data InitializeSettings = InitializeSettings
    { bedFiles :: [FilePath]
    , chainLengthsFile :: FilePath
    , radius :: Int
    , resolution :: Int
    , binderCount :: Int
    , initAttempts :: Int
    }

initialize :: MonadRandom m => Int -> Int -> Int -> Int -> [[EnergyVector]] -> m (Maybe Dump)
initialize = undefined

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
      (Just dump) -> pure dump

run :: InitializeSettings -> SimulationSettings -> IO ()
run initializeSettings simulationSettings = do
    gen <- newStdGen
    (dump, gen') <- flip runRandT gen $ load initializeSettings
    dump' <- flip evalRandT gen $ simulate simulationSettings dump
    -- TODO: do something with the dump?
    pure ()

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

simulationParser :: Parser SimulationSettings
simulationParser = SimulationSettings
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

parser :: Parser (InitializeSettings, SimulationSettings)
parser = (,) <$> initializeParser <*> simulationParser
