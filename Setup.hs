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

downloadProtoDescription :: Verbosity -> IO ()
downloadProtoDescription verbosity =
    rawSystemExit verbosity "env"
        ["curl", "https://raw.githubusercontent.com/Motions/Format/master/message.proto",
        "-o", "src/message.proto"]

compileProtos :: Verbosity -> IO ()
compileProtos verbosity = do
    dir <- joinPath . (: ["src"]) <$> getCurrentDirectory
    withCurrentDirectory dir $
        rawSystemExit verbosity "env" ["hprotoc", "message.proto"]

handleProtos :: Verbosity -> IO ()
handleProtos verbosity = do
    downloadProtoDescription verbosity
    compileProtos verbosity

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = do
    fileExist <- doesFileExist fileName
    when fileExist $ removeFile fileName

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists fileName = do
    fileExists <- doesDirectoryExist fileName
    when fileExists $ removeDirectoryRecursive fileName

deleteCompiledProtos :: IO ()
deleteCompiledProtos = do
    removeDirectoryIfExists "src/Bio/Motions/Format/Proto"
    removeFileIfExists "src/Bio/Motions/Format/Proto.hs"

makePreHandle :: (Verbosity -> IO ()) -> a -> ConfigFlags -> IO HookedBuildInfo
makePreHandle function _  flags  = do
    function $ fromFlag $ configVerbosity flags
    return emptyHookedBuildInfo

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path act = do
    oldDir <- getCurrentDirectory
    setCurrentDirectory path
    ret <- act
    setCurrentDirectory oldDir
    return ret

main = defaultMainWithHooks
        simpleUserHooks {preConf = makePreHandle handleProtos
                        ,preClean = (\_ _ -> deleteCompiledProtos >> return emptyHookedBuildInfo)}
