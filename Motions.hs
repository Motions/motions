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
import Bio.Motions.Representation.Chain.Slow
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
import Data.Reflection
import GHC.Generics
import Specialise

import LoadCallbacks()

data GenerateSettings = GenerateSettings
    { bedFiles :: [FilePath]
    , chromosomeInfos :: [ChromosomeInfo]
    , bindersCounts :: [Int]
    , cellRadius :: Int
    , resolution :: Int
    , initAttempts :: Int
    , binderTypesNames :: [String]
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
    , enableLogging :: Bool
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
    , maxMoveRadSquared :: Int
    , maxChainDistSquared :: Int
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
            , ("enableLogging", "enable-logging")
            , ("verboseCallbacks", "verbose-callbacks")
            , ("simplePDB", "simple-pdb-output")
            , ("binaryOutput", "binary-output")
            , ("framesPerKF", "frames-per-keyframe")
            , ("requestedCallbacks", "enabled-callbacks")
            , ("freezePredicateString", "freeze-predicate")
            , ("generateSettings", "generate")
            , ("inputSettings", "load")
            , ("bedFiles", "bed-files")
            , ("chromosomeInfos", "chromosome-infos")
            , ("bindersCounts", "binders-counts")
            , ("cellRadius", "cell-radius")
            , ("initAttempts", "initialisation-attempts")
            , ("inputFiles", "input-files")
            , ("metaFile", "meta-file")
            , ("chromosomeName", "name")
            , ("chromosomeLength", "length")
            , ("binaryInput", "binary-input")
            , ("binderTypesNames", "binder-types-names")
            , ("skipFrames", "skip-frames")
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
                                       <*> v' .:? "max-move-radius" .!= 2
                                       <*> v' .:? "max-chain-segment-length" .!= 2
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
    loggingHandle = if enableLogging then Just stdout else Nothing

-- |Loads the initial state.
generate :: MonadIO m =>
      GenerateSettings
   -- ^The initialisation settings.
   -> Int
   -- ^Square of the maximum chain segment length.
   -> m (Dump, [String], [String])
   -- ^The resulting dump, chains names and binder types names
generate GenerateSettings{..} maxChainDistSquared = do
          let chromosomeInfosAsPairs = [(a, b) | ChromosomeInfo a b <- chromosomeInfos]
          energyVectors <- liftIO $ parseBEDs resolution chromosomeInfosAsPairs bedFiles
          let evLength = U.length . getEnergyVector . head . head $ energyVectors
          -- Both bindersCount and binderTypesNames lists do not include lamin type
          when (length bindersCounts /= length binderTypesNames) $ error
              "Wrong number of binder names (have you remembered not to specify \"Lamin\" type?)"
          when (evLength /= length bindersCounts + 1)
            $ error "The number of different binder types must be the same as the number of chain \
                     \ features (BED files) minus one (the lamin feature)."
          maybeDump <- liftIO $ initialise initAttempts cellRadius maxChainDistSquared bindersCounts energyVectors
          case maybeDump of
            Nothing -> error "Failed to initialise."
            Just dump -> pure (dump, map fst chromosomeInfosAsPairs, "Lamin":binderTypesNames)

-- See the "Specialise" module.
{-# RULES "simulate @IOChain @StandardScore @PDB @MWCIO/SPEC" E.simulate = simulate'IOChain'StandardScore'PDB'MWCIO #-}
{-# RULES "simulate @IOChain @StandardScore @Bin @MWCIO/SPEC" E.simulate = simulate'IOChain'StandardScore'Bin'MWCIO #-}
{-# RULES "simulate @SlowChain @StandardScore @PDB @MWCIO/SPEC" E.simulate = simulate'SlowChain'StandardScore'PDB'MWCIO #-}
{-# RULES "simulate @SlowChain @StandardScore @Bin @MWCIO/SPEC" E.simulate = simulate'SlowChain'StandardScore'Bin'MWCIO #-}

{-# ANN runSimulation ("HLint: ignore Redundant guard" :: String) #-}
runSimulation :: Settings -> IO Dump
runSimulation Settings{..} = dispatchScore
  where
    dispatchScore
        | "StandardScore" <- scoreName = dispatchRepr (Proxy :: Proxy StandardScore)
        | otherwise = fail "Invalid score"
    {-# INLINE dispatchScore #-}

    dispatchRepr :: _ => _ score -> IO Dump
    dispatchRepr scoreProxy
        | "IOChain" <- reprName = dispatchFastRepr scoreProxy (Proxy :: Proxy IOChainRepresentation)
        | "PureChain" <- reprName = dispatchFastRepr scoreProxy (Proxy :: Proxy PureChainRepresentation)
        | "SlowChain" <- reprName = dispatchSlowRepr scoreProxy
        | otherwise = fail "Invalid representation"
    {-# INLINE dispatchRepr #-}

    dispatchFastRepr :: _ => _ score -> _ repr -> IO Dump
    dispatchFastRepr scoreProxy reprProxy
        | (2, 2) <- (maxMoveRadSquared, maxChainDistSquared) = dispatchRandom scoreProxy reprProxy
        | otherwise = fail "Invalid maximum move radius or maximum chain distance: only sqrt(2) is allowed \
                            \ in IOChainRepresentation and PureChainRepresentation"
    {-# INLINE dispatchFastRepr #-}

    dispatchSlowRepr :: _ => _ score -> IO Dump
    dispatchSlowRepr scoreProxy =
        reifyNat (toInteger maxMoveRadSquared) $ \(Proxy :: Proxy r) ->
            reifyNat (toInteger maxChainDistSquared) $ \(Proxy :: Proxy d) ->
                dispatchRandom scoreProxy (Proxy :: Proxy (SlowChainRepresentation r d))
    {-# INLINE dispatchSlowRepr #-}

    dispatchRandom :: _ => _ score -> _ repr -> IO Dump
    dispatchRandom scoreProxy reprProxy
        | otherwise = dispatchInput scoreProxy reprProxy runMWCIO
    {-# INLINE dispatchRandom #-}

    dispatchInput :: _ => _ score -> Proxy repr -> (forall a. m a -> IO a) -> IO Dump
    dispatchInput scoreProxy (reprProxy :: _ repr) (random :: forall a. m a -> IO a) =
        case (generateSettings, inputSettings) of
          (Nothing, Nothing) ->
              error "The state initialisation method (\"generate\" or \"load\") was not specified."
          (Just _, Just _) ->
              error "Both \"generate\" and \"load\" methods provided. Choose one."
          (_, Just settings) -> if binaryInput settings
               then withBinaryInput settings $ \prod names binderTN -> do
                  dump <- seekBinaryKF prod $ skipFrames settings
                  withProd settings prod dump $ dispatchBackend scoreProxy reprProxy random names binderTN
               else withPDBInput settings maxChainDistSquared $ \prod names binderTN -> do
                  dump <- skipPDBInput prod $ skipFrames settings
                  withProd settings prod dump $ dispatchBackend scoreProxy reprProxy random names binderTN
          (Just settings, _) -> do
              (dump, names, binderNames) <- generate settings maxChainDistSquared
              dispatchBackend scoreProxy reprProxy random names binderNames MoveGenerator dump
      where
        InitialisationSettings{..} = initialisationSettings
        withProd :: _ => InputSettings -> p -> Dump ->
                         (forall p. MoveProducer m repr p => p -> Dump -> IO Dump) -> IO Dump
        withProd settings prod dump f = case moveSource settings of
                                          "generate" -> f MoveGenerator dump
                                          "input" -> f prod dump
                                          _ -> fail "invalid move-source"

    {-# INLINE dispatchInput #-}

    dispatchBackend :: _ => _ score -> _ repr -> (forall a. m a -> IO a)
                        -> [String] -> [String] -> producer -> Dump -> IO Dump
    dispatchBackend scoreProxy reprProxy random chainNames binderTypesNames producer dump
        | binaryOutput = run $ openBinaryOutput framesPerKF outSettings dump
        | otherwise = run $ openPDBOutput outSettings dump simplePDB writeIntermediatePDB
        where
            RunSettings'{..} = runSettings
            outSettings = OutputSettings{..}

            run :: _ => IO backend -> IO Dump
            run open = bracket open closeBackend $ \backend ->
                dispatchFinal scoreProxy reprProxy random backend producer dump
            {-# INLINE run #-}
    {-# INLINE dispatchBackend #-}

    dispatchFinal :: _ => _ score -> _ repr -> (forall a. m a -> IO a)
                    -> backend -> producer -> Dump -> IO Dump
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
