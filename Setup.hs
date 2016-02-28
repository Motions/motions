import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Directory

main = defaultMainWithHooks simpleUserHooks
    { preConf = makeExtLib
    , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
    }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env"
        ["cmake", "libprotostream/"]
    rawSystemExit verbosity "env"
        ["make", "--directory=libprotostream/"]
    return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    print $ extraLibDirs libBuild
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "/libprotostream/src") :
                        extraLibDirs libBuild
                }
            }
        }
    }
