import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Control.Monad.IO.Class
import           Control.Monad

type VersionNumber = String
type GhcVersion = String

hies :: [FilePath]
hies = [ "hie-8.2.2"
       , "hie-8.4.2" 
       , "hie-8.4.3" 
       , "hie-8.4.4"
       , "hie-8.6.1" 
       , "hie-8.6.2"]

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    phony "ghc" $ do
        ghc <- readGhcVersion
        command_ [] ghc ["--version"]
        putNormal "GHC"

    phony "submodules" buildSubmodules
    phony "cabal" $ readGhcVersion >>= installCabal

    phony "all"   helpMessage
    phony "help"  helpMessage

    phony "build" (need hies)

    phony "build-all" (need ["build", "build-docs"])

    forM_
        hies
        (\hie -> phony hie $ do
            need ["submodules", "cabal"]
            stackLocalDir <- getLocalBin
            let versionNumber = drop 4 hie
            buildHie stackLocalDir versionNumber
        )

    phony "build-docs" $ forM_ hies $ \hie -> do
        let versionNumber = drop 4 hie
        buildDoc versionNumber

    phony "build-copy-compiler-tool" $ forM_ hies $ \hie -> do 
        let versionNumber = drop 4 hie
        buildCopyCompilerTool versionNumber

    phony "test" $ forM_ hies $ \hie -> do 
        let versionNumber = drop 4 hie
        test versionNumber

readGhcVersion :: Action GhcVersion
readGhcVersion = do
    Stdout ghc' <- execStack ["path", "--compiler-exe"]
    return (init ghc')

getLocalBin :: Action FilePath
getLocalBin = do
    Stdout stackLocalDir' <- execStack ["path", "--local-bin"]
    return (init stackLocalDir')

buildSubmodules :: Action ()
buildSubmodules = do
    command_ [] "git" ["submodule", "sync"]
    command_ [] "git" ["submodule", "update", "--init"]

installCabal :: GhcVersion -> Action ()
installCabal ghc = do
    execStack_ ["install", "cabal-install"]
    execCabal_ ["update"]
    execCabal_ ["install", "Cabal-2.4.1.0", "--with-compiler=" ++ ghc]

installHappy :: VersionNumber -> Action ()
installHappy versionNumber = execStackWithGhcVersion versionNumber [ "install", "happy"]

buildHie :: FilePath -> VersionNumber -> Action ()
buildHie localBinDir versionNumber = do
    when (versionNumber `elem` ["hie-8.2.2", "hie-8.2.1"]) $
        execStackWithGhcVersion versionNumber ["install", "happy"]
    execStackWithGhcVersion versionNumber [ "build"]
    execStackWithGhcVersion versionNumber [ "install"]
    cmd_
        "cp"
        [ localBinDir </> "hie" <.> exe]
        [ localBinDir </> "hie-" ++ versionNumber <.> exe
        ]

buildCopyCompilerTool :: VersionNumber -> Action () 
buildCopyCompilerTool versionNumber = do 
    execStackWithGhcVersion versionNumber ["build", "--copy-compiler-tool"]

test :: VersionNumber -> Action () 
test versionNumber = execStackWithGhcVersion versionNumber ["test"]

buildDoc :: VersionNumber -> Action ()
buildDoc versionNumber = do
    execStackWithGhcVersion versionNumber ["install", "hoogle"]
    execStackWithGhcVersion versionNumber ["exec", "hoogle", "generate"]

helpMessage :: Action ()
helpMessage = putNormal "Let me help you!"

execStackWithGhcVersion :: VersionNumber -> [String] -> Action ()
execStackWithGhcVersion versionNumber args = do 
    let stackFile = "stack-" ++ versionNumber ++ ".yaml"
    command_ []
             "stack"
             (["--stack-yaml=" ++ stackFile] ++ args)

execStack :: CmdResult r => [String] -> Action r
execStack = command [] "stack" 

execStack_ :: [String] -> Action ()
execStack_ = command_ [] "stack" 

execCabal_ :: [String] -> Action ()
execCabal_ = command_ [] "cabal"