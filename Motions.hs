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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Bio.Motions.Types
import Bio.Motions.BED
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Chain
import Bio.Motions.Representation.Dump
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Discover
import Bio.Motions.Callback.StandardScore
import Bio.Motions.Callback.GyrationRadius()
import Bio.Motions.Format.Handle
import Bio.Motions.StateInitialisation
import Bio.Motions.Output
import Bio.Motions.Input
import Bio.Motions.PDB.Backend
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

data InitialisationSettings = InitialisationSettings
    { generateSettings :: Maybe GenerateSettings
    , inputSettings :: Maybe InputSettings
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
            , ("inputFiles", "input-files")
            , ("metaFile", "meta-file")
            , ("chromosomeName", "name")
            , ("chromosomeLength", "length")
            , ("binaryInput", "binary-input")
            , ("moveSource", "move-source")
            ]

instance FromJSON GenerateSettings where
    parseJSON = genericParseJSON'

instance FromJSON InputSettings where
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

mkRunSettings :: RunSettings' -> backend -> producer -> E.RunSettings repr score backend producer
mkRunSettings RunSettings'{..} outputBackend producer = E.RunSettings{..}
  where
    allPreCallbacks = $(allCallbacks Pre)
    allPostCallbacks = $(allCallbacks Post)
    freezePredicate = case freezePredicateString of
        Just str -> either (fail . show) id $ P.parse freezePredicateParser "<input>" str
        Nothing -> freezeNothing

generate :: (MonadIO m) => GenerateSettings -> m (Dump, [String])
generate GenerateSettings{..} = do
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

runSimulation :: Settings -> IO Dump
runSimulation Settings{..} = dispatchScore
  where
    dispatchScore
        | "StandardScore" <- scoreName = dispatchRepr (Proxy :: Proxy StandardScore)
        | otherwise = fail "Invalid score"
    {-# INLINE dispatchScore #-}

    dispatchRepr :: _ => _ score -> IO Dump
    dispatchRepr scoreProxy
        | "IOChain" <- reprName = dispatchRandom scoreProxy (Proxy :: Proxy IOChainRepresentation)
        | "PureChain" <- reprName = dispatchRandom scoreProxy (Proxy :: Proxy PureChainRepresentation)
        | otherwise = fail "Invalid representation"
    {-# INLINE dispatchRepr #-}

    dispatchRandom :: _ => _ score -> _ repr -> IO Dump
    dispatchRandom scoreProxy reprProxy
        | otherwise = dispatchInput scoreProxy reprProxy runMWCIO
    {-# INLINE dispatchRandom #-}

    dispatchInput :: forall (w_ :: * -> *)
                                (w_1 :: * -> *)
                                score
                                repr
                                (m :: * -> *).
                         (Random m Double,
                          RandomRepr m repr,
                          Score score, Callback 'Pre score,
                          Representation m repr, MonadIO m,
                          MonadRandom m) =>
                         w_ score -> w_1 repr -> (forall a. m a -> IO a) -> IO Dump
    dispatchInput scoreProxy reprProxy random =
        case (generateSettings, inputSettings) of
          (Nothing, Nothing) ->
              error "The state initialisation method (\"generate\" or \"load\") was not specified."
          (Just _, Just _) ->
              error "Both \"generate\" and \"load\" methods provided. Choose one."
          (_, Just settings) -> if binaryInput settings then withBinaryInput settings $ \prod _  names -> do
                                    dump <- seekBinaryKF prod 0 -- TODO 0
                                    withProd settings prod dump $ dispatchBackend scoreProxy reprProxy random names
                                else withPDBInput settings $ \prod dump names -> do
                                    return ()    --TODO skip here
                                    withProd settings prod dump $ dispatchBackend scoreProxy reprProxy random names
          (Just settings, _) -> do
              (dump, names) <- generate settings
              dispatchBackend scoreProxy reprProxy random names MoveGenerator dump
      where
          InitialisationSettings{..} = initialisationSettings
          withProd :: _ => InputSettings -> p -> Dump -> (forall p. MoveProducer m repr p => p -> Dump -> IO Dump) -> IO Dump
          withProd settings prod dump f = case moveSource settings of
                                            "generate" -> f MoveGenerator dump
                                            "input" -> f prod dump
                                            _ -> fail "invalid move-source"

    {-# INLINE dispatchInput #-}

    dispatchBackend :: _ => _ score -> _ repr -> (forall a. m a -> IO a) -> [String] -> producer -> Dump -> IO Dump
    dispatchBackend scoreProxy reprProxy random chainNames producer dump
        | binaryOutput = run $ openBinaryOutput framesPerKF outSettings dump
        | otherwise = run $ openPDBOutput outSettings dump simplePDB writeIntermediatePDB
                                callbacksHandle verboseCallbacks
        where
            callbacksHandle = stdout    --TODO this should be a path or something, and the handle
                                        -- would then be managed(open/close) by the backend
            RunSettings'{..} = runSettings
            outSettings = OutputSettings{..}

            run :: _ => IO backend -> IO Dump
            run open = bracket open closeBackend $ \backend ->
                dispatchFinal scoreProxy reprProxy random backend producer dump
            {-# INLINE run #-}
    {-# INLINE dispatchBackend #-}

    dispatchFinal :: _ => _ score -> _ repr -> (forall a. m a -> IO a) -> backend -> producer -> Dump -> IO Dump
    dispatchFinal (_ :: _ score) (_ :: _ repr) random backend producer dump =
        random $ E.simulate (mkRunSettings runSettings backend producer :: E.RunSettings repr score _ _) dump
    {-# INLINE dispatchFinal #-}

run :: Settings -> IO ()
run settings@Settings{..} = do
    when (simplePDB runSettings) $
        putStrLn "Warning: when using \"simple-pdb-output: True\" with 3 or more different binder types \
                  \ it won't be possible to use the resulting output as initial state later."
    _ <- runSimulation settings
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
