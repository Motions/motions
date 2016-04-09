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
import Data.Aeson.Types
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
    { pdbFile :: FilePath
    , numSteps :: Int
    , writeIntermediatePDB :: Bool
    , verboseCallbacks :: Bool
    , simplePDB :: Bool
    , requestedCallbacks :: [String]
    , freezeFile :: Maybe FilePath
    } deriving Generic

data Settings = Settings
    { runSettings :: RunSettings'
    , reprName :: String
    , scoreName :: String
    , initialisationSettings :: InitialisationSettings
    }

labelModifier :: String -> String
labelModifier s = fromMaybe s $ lookup s assoc
  where assoc = [ ("pdbFile", "output-file")
                , ("numSteps", "steps")
                , ("writeIntermediatePDB", "write-intermediate-frames")
                , ("verboseCallbacks", "verbose-callbacks")
                , ("simplePDB", "simple-pdb-output")
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
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = labelModifier }

instance FromJSON LoadStateSettings

instance FromJSON InitialisationSettings where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = labelModifier }

instance FromJSON RunSettings' where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = labelModifier }

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
load InitialisationSettings{..} =
    case (generateSettings, loadStateSettings) of
      (Nothing, Nothing) -> error "At least one of the \"generate\" or \"load\" keys must be provided"
      (_, Just LoadStateSettings{..}) -> liftIO $ do
          meta <- either (error . ("Meta file read error: " ++)) pure =<< withFile metaFile ReadMode readPDBMeta
          pdbHandles <- mapM (`openFile` ReadMode) pdbFiles
          dump <- either (error . ("PDB read error: " ++)) pure =<< readPDB pdbHandles meta
          mapM_ hClose pdbHandles
          pure dump
      (Just GenerateSettings{..}, _) -> do
          energyVectors <- liftIO $ parseBEDs resolution chainLengths bedFiles
          let evLength = U.length . getEnergyVector . head . head $ energyVectors
          when (evLength /= length bindersCounts + 1) $ error binderErrorMsg
          maybeDump <- initialise initAttempts radius bindersCounts energyVectors
          case maybeDump of
            Nothing -> error "Failed to initialise"
            Just dump -> pure dump
  where
    binderErrorMsg = "The number of different binder types must be the same as the number of chain features \
                      \ (BED files) minus one (the lamin feature)."

run :: Settings -> IO ()
run Settings{..} = do
    when (simplePDB runSettings) $
        putStrLn $ "Warning: when using \"simple-pdb-output: True\" with 3 or more different binder types"
                   ++ " it won't be possible to use the resulting output as initial state later."
    gen <- newStdGen
    _ <- flip evalRandT gen $ do
        dump <- load initialisationSettings
        runScore runRepr runSettings dump
    -- TODO: do something with the dump?
    pure ()
  where
    RunScore runScore = fromMaybe (error "Invalid score") $ lookup scoreName scoreMap
    RunRepr runRepr = fromMaybe (error "Invalid representation") $ lookup reprName reprMap

main :: IO ()
main = do
    configFile <- execParser
                    (info (helper <*> inputFileParser)
                          (fullDesc <> progDesc "Perform a MCMC simulation of chromatin movements"))
    config <- decodeFileEither configFile
    config' <- either (error . show) pure config
    run config'

inputFileParser :: O.Parser FilePath
inputFileParser = strOption
                  (long "config"
                  <> short 'c'
                  <> metavar "YAML-CONFIG-FILE"
                  <> help "File containing the configuration necessary to run the simulation.")
