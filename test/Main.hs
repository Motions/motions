module Main where

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import Test.Hspec.Formatters.Jenkins (xmlFormatter)

import System.IO
import System.Exit
import System.Environment
import Control.Monad

import qualified Spec

main :: IO ()
main = do
    jenkins <- lookupEnv "BUILD_TAG"
    let c = case jenkins of
            Just x | take 7 x == "jenkins" -> defaultConfig {
                     configFormatter = Just xmlFormatter
                     , configOutputFile = Right "results.xml"
                     }
            _ -> defaultConfig

    summary <- hspecWithResult c Spec.spec
    unless (summaryFailures summary == 0) exitFailure
