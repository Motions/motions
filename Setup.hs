import Distribution.Simple
import Distribution.PackageDescription
import System.Process
import Control.Monad

verboseCommand :: String -> IO()
verboseCommand command = do
    putStrLn command
    void $ runCommand command

deleteCompiledProtos :: IO()
deleteCompiledProtos =
    verboseCommand "rm -rf src/Bio/Motions/Format/Proto src/Bio/Motions/Format/Proto.hs"

compileProtos :: IO ()
compileProtos =
    verboseCommand "cd src; hprotoc message.proto; cd .."

main = defaultMainWithHooks
        simpleUserHooks {preConf = \_ _ -> compileProtos >> return emptyHookedBuildInfo,
                         preClean = \_ _ -> deleteCompiledProtos >> return emptyHookedBuildInfo}
