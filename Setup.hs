import Control.Monad
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error
import System.Process

main = defaultMainWithHooks simpleUserHooks
    { preConf = \a f ->  makePreHandle handleProtos f >> makeLibProtostream a f
    , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
    , preClean = \_ _ -> deleteCompiledProtos >> return emptyHookedBuildInfo
    }

makeLibProtostream :: Args -> ConfigFlags -> IO HookedBuildInfo
makeLibProtostream _ flags = do
    dir <- joinPath . (: ["libprotostream"]) <$> getCurrentDirectory
    createDirectoryIfMissing False dir
    withCurrentDirectory dir $ do
        prefix <- runStdout ["git", "rev-parse", "--show-prefix"]
        when (prefix /= "\n") $ -- not in the root of the git working directory
            run ["git", "clone", "https://github.com/Motions/libprotostream.git", "."]
        local <- runStdout ["git", "rev-parse", "@"]
        upstream <- runStdout ["git", "rev-parse", "origin/master"]
        when (local /= upstream) $ do
            run ["git", "clean", "-f", "-d"]
            run ["git", "reset", "--hard", "origin/master"]
        run ["git", "checkout", "."]

    let buildDir = joinPath [dir, "build"]
    createDirectoryIfMissing False buildDir

    withCurrentDirectory buildDir $ do
        run ["cmake", ".."]
        run ["make", "cprotostream_static"]

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
removeFileIfExists fileName = catchIOError(removeFile fileName) $
    unless <$> isDoesNotExistError <*> ioError

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists dirName = catchIOError(removeDirectory dirName) $
    unless <$> isDoesNotExistError <*> ioError

deleteCompiledProtos :: IO ()
deleteCompiledProtos = do
    let dir = joinPath ["src", "Bio", "Motions", "Format"]
    removeDirectoryIfExists $ joinPath [dir, "Proto"]
    removeFileIfExists $ joinPath [dir, "Proto.hs"]

makePreHandle :: (Verbosity -> IO ()) -> ConfigFlags -> IO HookedBuildInfo
makePreHandle function flags  = do
    function $ fromFlag $ configVerbosity flags
    return emptyHookedBuildInfo

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path act = do
    oldDir <- getCurrentDirectory
    setCurrentDirectory path
    ret <- act
    setCurrentDirectory oldDir
    pure ret
