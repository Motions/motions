{- |
Module      : Main
Description : Contains the main function.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
import Options.Applicative as O
import Data.Proxy
import Data.Maybe
import Data.Yaml
import Data.Aeson.Types as J
import GHC.Generics

import LoadCallbacks()

data GenerateSettings = GenerateSettings
    { bedFiles :: [FilePath]
    , chainLengths :: [Int]
    , bindersCounts :: [Int]
    , radius :: Int
    , resolution :: Int
    , initAttempts :: Int
    } deriving Generic

data LoadStateSettings = LoadStateSettings
    { pdbFiles :: [FilePath]
    , metaFile :: FilePath
    } deriving Generic

data InitialisationSettings = InitialisationSettings
    { generateSettings :: Maybe GenerateSettings
    , loadStateSettings :: Maybe LoadStateSettings
    } deriving Generic

data RunSettings' = RunSettings'
    { outputPrefix :: FilePath
    , numSteps :: Int
    , writeIntermediatePDB :: Bool
    , verboseCallbacks :: Bool
    , simplePDB :: Bool
    , binaryOutput :: Bool
    , requestedCallbacks :: [String]
    , freezeFile :: Maybe FilePath
    } deriving Generic

data Settings = Settings
    { runSettings :: RunSettings'
    , reprName :: String
    , scoreName :: String
    , initialisationSettings :: InitialisationSettings
    }

genericParseJSON' :: (Generic a, GFromJSON (Rep a)) => Value -> J.Parser a
genericParseJSON' = genericParseJSON $ defaultOptions { fieldLabelModifier = labelModifier }
  where
    labelModifier s = fromMaybe s $ lookup s assoc
    assoc = [ ("pdbFile", "output-file")
            , ("numSteps", "steps")
            , ("writeIntermediatePDB", "write-intermediate-frames")
            , ("verboseCallbacks", "verbose-callbacks")
            , ("simplePDB", "simple-pdb-output")
            , ("binaryOutput", "binary-output")
            , ("requestedCallbacks", "enabled-callbacks")
            , ("freezeFile", "freeze-file")
            , ("generateSettings", "generate")
            , ("loadStateSettings", "load")
            , ("bedFiles", "bed-files")
            , ("chainLengths", "chain-lengths")
            , ("bindersCounts", "binders-counts")
            , ("initAttempts", "initialisation-attempts")
            , ("pdbFiles", "pdb-files")
            , ("metaFile", "meta-file")
            ]

instance FromJSON GenerateSettings where
    parseJSON = genericParseJSON'

instance FromJSON LoadStateSettings where
    parseJSON = genericParseJSON'

instance FromJSON InitialisationSettings where
    parseJSON = genericParseJSON'

instance FromJSON RunSettings' where
    parseJSON = genericParseJSON'

instance FromJSON Settings where
    parseJSON v@(Object v') = Settings <$> parseJSON v
                                       <*> v' .:? "representation" .!= "IOChain"
                                       <*> v' .:? "score" .!= "StandardScore"
                                       <*> parseJSON v
    parseJSON invalid = typeMismatch "Object" invalid


mkRunSettings :: RunSettings' -> E.RunSettings repr score
mkRunSettings RunSettings'{..} = E.RunSettings{..}
  where
    allPreCallbacks = $(allCallbacks Pre)
    allPostCallbacks = $(allCallbacks Post)

type Run' = RunSettings' -> Dump -> IO Dump
type Run score = Proxy score -> Run'

newtype RunRepr = RunRepr { runRepr :: forall score. Score score => Run score }
newtype RunScore = RunScore { runScore :: (forall score. Score score => Run score) -> Run' }

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

load :: (MonadIO m, MonadRandom m) => InitialisationSettings -> m Dump
load InitialisationSettings{..} =
    case (generateSettings, loadStateSettings) of
      (Nothing, Nothing) ->
          error "The state initialisation method (\"generate\" or \"load\") was not specified."
      (Just _, Just _) ->
          error "Both \"generate\" and \"load\" methods provided. Choose one."
      (_, Just LoadStateSettings{..}) -> liftIO $ do
          meta <- either (error . ("Meta file read error: " ++)) pure =<< withFile metaFile ReadMode readPDBMeta
          pdbHandles <- mapM (`openFile` ReadMode) pdbFiles
          dump <- either (error . ("PDB read error: " ++)) pure =<< readPDB pdbHandles meta
          mapM_ hClose pdbHandles
          pure dump
      (Just GenerateSettings{..}, _) -> do
          energyVectors <- liftIO $ parseBEDs resolution chainLengths bedFiles
          let evLength = U.length . getEnergyVector . head . head $ energyVectors
          when (evLength /= length bindersCounts + 1)
            $ error "The number of different binder types must be the same as the number of chain \
                     \ features (BED files) minus one (the lamin feature)."
          maybeDump <- initialise initAttempts radius bindersCounts energyVectors
          case maybeDump of
            Nothing -> error "Failed to initialise."
            Just dump -> pure dump

{-# SPECIALISE E.simulate :: E.RunSettings IOChainRepresentation StandardScore -> Dump -> IO Dump #-}
{-# SPECIALISE E.simulate :: E.RunSettings PureChainRepresentation StandardScore -> Dump -> IO Dump #-}
runSimulation :: Settings -> Dump -> IO Dump
runSimulation Settings{..}
    -- Make the GHC use the specialised dictionaries.
    | "IOChain" <- reprName,
      "StandardScore" <- scoreName =
        E.simulate (mkRunSettings runSettings :: E.RunSettings IOChainRepresentation StandardScore)
    | "PureChain" <- reprName,
      "StandardScore" <- scoreName =
        E.simulate (mkRunSettings runSettings :: E.RunSettings PureChainRepresentation StandardScore)
    | otherwise = runScore runRepr runSettings
  where
    RunScore runScore = fromMaybe (error "Invalid score") $ lookup scoreName scoreMap
    RunRepr runRepr = fromMaybe (error "Invalid representation") $ lookup reprName reprMap

run :: Settings -> IO ()
run settings@Settings{..} = do
    when (simplePDB runSettings) $
        putStrLn "Warning: when using \"simple-pdb-output: True\" with 3 or more different binder types \
                  \ it won't be possible to use the resulting output as initial state later."
    dump <- load initialisationSettings
    _ <- runSimulation settings dump
    -- TODO: do something with the dump?
    pure ()

main :: IO ()
main = do
    configFile <- execParser
                    (info (helper <*> inputFileParser)
                          (fullDesc <> progDesc "Perform a MCMC simulation of chromatin movements"))
    config <- decodeFileEither configFile
    either (error . show) run config

inputFileParser :: O.Parser FilePath
inputFileParser = strOption
                  (long "config"
                  <> short 'c'
                  <> metavar "YAML-CONFIG-FILE"
                  <> help "File containing the configuration necessary to run the simulation.")
