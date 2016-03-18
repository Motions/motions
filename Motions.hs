{- |
Module      : Main
Description : Contains the main function.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import Bio.Motions.Types
import Bio.Motions.BED
import Bio.Motions.Representation.Chain
import Bio.Motions.Representation.Dump
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Discover
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius()
import Bio.Motions.StateInitialisation
import Bio.Motions.PDB.Read
import Bio.Motions.PDB.Meta
import qualified Bio.Motions.Engine as E

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad
import qualified Data.Vector.Unboxed as U
import Options.Applicative
import Data.Proxy
import Data.Maybe
import Data.List

import LoadCallbacks()

data GenerateSettings = GenerateSettings
    { bedFiles :: [FilePath]
    , chainLengthsFile :: FilePath
    , bindersCountsFile :: FilePath
    , radius :: Int
    , resolution :: Int
    , initAttempts :: Int
    }

data LoadStateSettings = LoadStateSettings
    { pdbFiles :: [FilePath]
    , metaFile :: FilePath
    }

data InitialisationSettings = Generate GenerateSettings
                            | Load LoadStateSettings

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
    , simplePDB :: Bool
    , freezeFile :: Maybe FilePath
    , requestedCallbacksFile :: FilePath
    }

mkRunSettings :: RunSettings' -> E.RunSettings repr score
mkRunSettings RunSettings'{..} = E.RunSettings{..}
  where
    allPreCallbacks = $(allCallbacks Pre)
    allPostCallbacks = $(allCallbacks Post)

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
        E.simulate (mkRunSettings rs :: E.RunSettings PureChainRepresentation score)
    runIOChain = RunRepr $ \(_ :: _ score) rs ->
        E.simulate (mkRunSettings rs :: E.RunSettings IOChainRepresentation score)

scoreMap :: [(String, RunScore)]
scoreMap = [ ("StandardScore", runStandardScore)
           ]
  where
    runStandardScore = RunScore $ \run -> run (Proxy :: Proxy StandardScore)

loadInts :: FilePath -> IO [Int]
loadInts path = withFile path ReadMode $ fmap (map read . words) . hGetLine

load :: (MonadIO m, MonadRandom m) => InitialisationSettings -> m Dump
load (Generate GenerateSettings{..}) = do
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
load (Load LoadStateSettings{..}) = liftIO $ do
    meta <- either (error . ("Meta file read error " ++)) pure =<< withFile metaFile ReadMode readPDBMeta
    pdbHandles <- mapM (`openFile` ReadMode) pdbFiles
    dump <- either (error . ("PDB read error " ++)) pure =<< readPDB pdbHandles meta
    mapM_ hClose pdbHandles
    pure dump

run :: SimulationSettings -> InitialisationSettings -> IO ()
run simulationSettings initialisationSettings = do
    when (simplePDB . runSettings $ simulationSettings) $
        print $ "Warning: when using --simple-pdb with 3 or more different binder types"
                ++ " it won't be possible to use the resulting output as initial state later."
    gen <- newStdGen
    flip evalRandT gen $ do
        dump <- load initialisationSettings
        runSimulation simulationSettings dump
    -- TODO: do something with the dump?
    pure ()

runSimulation :: RandomGen gen => SimulationSettings -> Dump -> RandT gen IO Dump
runSimulation SimulationSettings{..} = runScore runRepr runSettings
  where
    RunScore runScore = fromMaybe (error "Invalid score") $ lookup scoreName scoreMap
    RunRepr runRepr = fromMaybe (error "Invalid representation") $ lookup reprName reprMap

main :: IO ()
main = uncurry run =<< execParser
    (info (helper <*> parser)
          (fullDesc <> progDesc "Perform a MCMC simulation of chromatin movements"))

initialisationParser :: Parser InitialisationSettings
initialisationParser = subparser
    $  command "generate" (info (helper <*> (Generate <$> generateParser))
        (progDesc "Generate an initial random state"))
    <> command "load" (info (helper <*> (Load <$> loadParser))
        (progDesc "Load initial state from PDB files"))
    <> metavar "INITIALISE-COMMAND"

generateParser :: Parser GenerateSettings
generateParser = GenerateSettings
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

loadParser :: Parser LoadStateSettings
loadParser = LoadStateSettings
    <$> some (strArgument $ metavar "PDB files...")
    <*> strOption
        (long "metafile"
        <> short 'm'
        <> metavar "PDB-META-FILE"
        <> help "File containing meta information about PDB-state conversion")

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
    <*> switch
        (long "simple-pdb")
    <*> optional (strOption
        (long "freezefile"
        <> metavar "FREEZE_FILE"
        <> help "File containing the ranges of frozen beads' indices"))
    <*> strOption
        (long "callbacks"
         <> short 'c'
         <> metavar "CALLBACKS"
         <> help "File containing a newline-separated list of enabled callback names")

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

parser :: Parser (SimulationSettings, InitialisationSettings)
parser = (,) <$> simulationParser <*> initialisationParser
