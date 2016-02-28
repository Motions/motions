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
    let verbosity = fromFlag $ configVerbosity flags
    dir <- joinPath . (: ["libprotostream"]) <$> getCurrentDirectory

    dirExists <- doesDirectoryExist dir
    unless dirExists $
        rawSystemExit verbosity "env"
            ["git", "clone", "https://github.com/Motions/libprotostream.git", dir]

    withCurrentDirectory dir $ do
        rawSystemExit verbosity "env" ["cmake", "."]
        rawSystemExit verbosity "env" ["make", "protostream"]

    return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- joinPath . (: ["libprotostream", "src"]) <$> getCurrentDirectory
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
