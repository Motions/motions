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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Bio.Motions.Types
import Bio.Motions.BED
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Chain
import Bio.Motions.Representation.Dump
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Discover
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius()
import Bio.Motions.Format.Handle
import Bio.Motions.StateInitialisation
import Bio.Motions.Output
import Bio.Motions.PDB.Backend
import Bio.Motions.PDB.Read
import Bio.Motions.PDB.Meta
import qualified Bio.Motions.Engine as E
import Bio.Motions.Utils.FreezePredicateParser
import Bio.Motions.Utils.Random
import Text.Parsec as P

import Control.Exception
import System.IO
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Vector.Unboxed as U
import Options.Applicative as O
import Data.Proxy
import Data.Maybe
import Data.Yaml
import Data.Aeson.Types as J
import GHC.Generics
import Specialise

import LoadCallbacks()

data GenerateSettings = GenerateSettings
    { bedFiles :: [FilePath]
    , chromosomeInfos :: [ChromosomeInfo]
    , bindersCounts :: [Int]
    , radius :: Int
    , resolution :: Int
    , initAttempts :: Int
    } deriving Generic

data ChromosomeInfo = ChromosomeInfo
    { chromosomeName :: String
    , chromosomeLength :: Int
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
    , simulationName :: String
    , simulationDescription :: String
    , numSteps :: Int
    , writeIntermediatePDB :: Bool
    , verboseCallbacks :: Bool
    , simplePDB :: Bool
    , binaryOutput :: Bool
    , framesPerKF :: Int
    , requestedCallbacks :: [String]
    , freezePredicateString :: Maybe String
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
    assoc = [ ("simulationDescription", "description")
            , ("simulationName", "name")
            , ("outputPrefix", "output-prefix")
            , ("numSteps", "steps")
            , ("writeIntermediatePDB", "write-intermediate-frames")
            , ("verboseCallbacks", "verbose-callbacks")
            , ("simplePDB", "simple-pdb-output")
            , ("binaryOutput", "binary-output")
            , ("framesPerKF", "frames-per-keyframe")
            , ("requestedCallbacks", "enabled-callbacks")
            , ("freezePredicateString", "freeze-predicate")
            , ("generateSettings", "generate")
            , ("loadStateSettings", "load")
            , ("bedFiles", "bed-files")
            , ("chromosomeInfos", "chromosome-infos")
            , ("bindersCounts", "binders-counts")
            , ("initAttempts", "initialisation-attempts")
            , ("pdbFiles", "pdb-files")
            , ("metaFile", "meta-file")
            , ("chromosomeName", "name")
            , ("chromosomeLength", "length")
            ]

instance FromJSON GenerateSettings where
    parseJSON = genericParseJSON'

instance FromJSON LoadStateSettings where
    parseJSON = genericParseJSON'

instance FromJSON InitialisationSettings where
    parseJSON = genericParseJSON'

instance FromJSON RunSettings' where
    parseJSON = genericParseJSON'

instance FromJSON ChromosomeInfo where
    parseJSON = genericParseJSON'

instance FromJSON Settings where
    parseJSON v@(Object v') = Settings <$> parseJSON v
                                       <*> v' .:? "representation" .!= "IOChain"
                                       <*> v' .:? "score" .!= "StandardScore"
                                       <*> parseJSON v
    parseJSON invalid = typeMismatch "Object" invalid

mkRunSettings :: RunSettings' -> backend -> E.RunSettings repr score backend
mkRunSettings RunSettings'{..} outputBackend = E.RunSettings{..}
  where
    allPreCallbacks = $(allCallbacks Pre)
    allPostCallbacks = $(allCallbacks Post)
    freezePredicate = case freezePredicateString of
        Just str -> either (fail . show) id $ P.parse freezePredicateParser "<input>" str
        Nothing -> freezeNothing

load :: (MonadIO m) => InitialisationSettings -> m (Dump, [String])
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
          pure (dump, getChainNames meta)
      (Just GenerateSettings{..}, _) -> do
          let chromosomeInfosAsPairs = [(a, b) | ChromosomeInfo a b <- chromosomeInfos]
          energyVectors <- liftIO $ parseBEDs resolution chromosomeInfosAsPairs bedFiles
          let evLength = U.length . getEnergyVector . head . head $ energyVectors
          when (evLength /= length bindersCounts + 1)
            $ error "The number of different binder types must be the same as the number of chain \
                     \ features (BED files) minus one (the lamin feature)."
          maybeDump <- liftIO $ initialise initAttempts radius bindersCounts energyVectors
          case maybeDump of
            Nothing -> error "Failed to initialise."
            Just dump -> pure (dump, map fst chromosomeInfosAsPairs)

-- See the "Specialise" module.
{-# RULES "simulate @IOChain @StandardScore @PDB @MWCIO/SPEC" E.simulate = simulate'IOChain'StandardScore'PDB'MWCIO #-}
{-# RULES "simulate @IOChain @StandardScore @Bin @MWCIO/SPEC" E.simulate = simulate'IOChain'StandardScore'Bin'MWCIO #-}

runSimulation :: Settings -> Dump -> [String] -> IO Dump
runSimulation Settings{..} dump chainNames = dispatchScore dump
  where
    dispatchScore dump
        | "StandardScore" <- scoreName = dispatchRepr (Proxy :: Proxy StandardScore) dump
        | otherwise = fail "Invalid score"
    {-# INLINE dispatchScore #-}

    dispatchRepr :: _ => _ score -> Dump -> IO Dump
    dispatchRepr scoreProxy dump
        | "IOChain" <- reprName = dispatchRandom scoreProxy (Proxy :: Proxy IOChainRepresentation) dump
        | "PureChain" <- reprName = dispatchRandom scoreProxy (Proxy :: Proxy PureChainRepresentation) dump
        | otherwise = fail "Invalid representation"
    {-# INLINE dispatchRepr #-}

    dispatchRandom :: _ => _ score -> _ repr -> Dump -> IO Dump
    dispatchRandom scoreProxy reprProxy dump
        | otherwise = dispatchBackend scoreProxy reprProxy runMWCIO dump
    {-# INLINE dispatchRandom #-}

    dispatchBackend :: _ => _ score -> _ repr -> (forall a. m a -> IO a) -> Dump -> IO Dump
    dispatchBackend scoreProxy reprProxy random dump
        | binaryOutput = run $ openBinaryOutput framesPerKF outSettings dump chainNames
        | otherwise = run $ openPDBOutput outSettings dump chainNames simplePDB writeIntermediatePDB
                                callbacksHandle verboseCallbacks
        where
            callbacksHandle = stdout    --TODO this should be a path or something, and the handle
                                        -- would then be managed(open/close) by the backend
            RunSettings'{..} = runSettings
            outSettings = OutputSettings{..}

            run :: _ => IO backend -> IO Dump
            run open = bracket open closeBackend $ \backend ->
                dispatchFinal scoreProxy reprProxy random backend dump
            {-# INLINE run #-}
    {-# INLINE dispatchBackend #-}

    dispatchFinal :: _ => _ score -> _ repr -> (forall a. m a -> IO a) -> backend -> Dump -> IO Dump
    dispatchFinal (_ :: _ score) (_ :: _ repr) random backend dump =
        random $ E.simulate (mkRunSettings runSettings backend :: E.RunSettings repr score _) dump
    {-# INLINE dispatchFinal #-}

run :: Settings -> IO ()
run settings@Settings{..} = do
    when (simplePDB runSettings) $
        putStrLn "Warning: when using \"simple-pdb-output: True\" with 3 or more different binder types \
                  \ it won't be possible to use the resulting output as initial state later."
    (dump, chainNames) <- load initialisationSettings
    _ <- runSimulation settings dump chainNames
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
