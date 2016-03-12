import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Process
import System.Exit
import System.FilePath
import System.Directory
import Control.Monad

downloadProtoDescription :: Verbosity -> IO()
downloadProtoDescription verbosity =
    rawSystemExit verbosity "env" ["curl", "https://raw.githubusercontent.com/Motions/Format/master/message.proto", "-o message.proto"]


compileProtos :: Verbosity -> IO ()
compileProtos verbosity = do
    dir <- joinPath . (: ["src"]) <$> getCurrentDirectory
    withCurrentDirectory dir $
        rawSystemExit verbosity "env" ["hprotoc", "message.proto"]

handleProtos :: Verbosity -> IO()
handleProtos verbosity = do
    downloadProtoDescription verbosity
    compileProtos verbosity

deleteCompiledProtos :: Verbosity -> IO()
deleteCompiledProtos verbosity = do
    rawSystemExit verbosity "env" ["rm", "-rf", "src/Bio/Motions/Format/Proto", "src/Bio/Motions/Format/Proto.hs"]

class HasVerbosity a where
    genericGetVerbosity :: a -> Verbosity

instance HasVerbosity ConfigFlags where
    genericGetVerbosity flags = fromFlag $ configVerbosity flags

instance HasVerbosity CleanFlags where
    genericGetVerbosity flags = fromFlag $ cleanVerbosity flags

makePreHandle :: (HasVerbosity b) => (Verbosity -> IO()) -> a -> b -> IO HookedBuildInfo
makePreHandle function _  flags  = do
    function $ genericGetVerbosity flags
    return emptyHookedBuildInfo

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path act = do
    oldDir <- getCurrentDirectory
    setCurrentDirectory path
    ret <- act
    setCurrentDirectory oldDir
    return ret

main = defaultMainWithHooks
        simpleUserHooks {preConf = makePreHandle handleProtos,
                         preClean = makePreHandle deleteCompiledProtos}
