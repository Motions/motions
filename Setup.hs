import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils

import System.Directory
import System.FilePath
import Data.Maybe
import Control.Monad

main = defaultMainWithHooks simpleUserHooks
    { preConf = makeExtLib
    , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
    }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
    dir <- joinPath . (: ["libprotostream"]) <$> getCurrentDirectory
    createDirectoryIfMissing False dir

    withCurrentDirectory dir $ do
        prefix <- runStdout ["git", "rev-parse", "--show-prefix"]
        when (prefix /= "\n") $
            run ["git", "clone", "https://github.com/Motions/libprotostream.git", "."]
        local <- runStdout ["git", "rev-parse", "@"]
        upstream <- runStdout ["git", "rev-parse", "origin/master"]
        when (local /= upstream) $ do
            run ["git", "clean", "-f", "-d"]
            run ["git", "reset", "--hard", "origin/master"]
        run ["git", "checkout", "."]

    let buildDir = joinPath . (: ["build"]) $ dir
    createDirectoryIfMissing False buildDir

    withCurrentDirectory buildDir $ do
        rawSystemExit verbosity "env" ["cmake", ".."]
        rawSystemExit verbosity "env" ["make", "cprotostream_static"]

    return emptyHookedBuildInfo
  where
    run = rawSystemExit verbosity "env"
    runStdout = rawSystemStdout verbosity "env"
    verbosity = fromFlag $ configVerbosity flags

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- joinPath . (: ["libprotostream", "build"]) <$> getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = dir : extraLibDirs libBuild
                }
            }
        }
    }

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path act = do
    oldDir <- getCurrentDirectory
    setCurrentDirectory path
    ret <- act
    setCurrentDirectory oldDir
    pure ret
