#!/usr/bin/env stack
-- stack --resolver lts-12.21 --install-ghc runghc --package shake

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Control.Monad

type VersionNumber = String
type GhcPath = String

hies :: [FilePath]
hies = [ "hie-8.2.1"
       , "hie-8.2.2"
       , "hie-8.4.2" 
       , "hie-8.4.3" 
       , "hie-8.4.4"
       , "hie-8.6.1" 
       , "hie-8.6.2"
       ]

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
        phony "ghc" $ do
            ghc <- readGhcPath
            command_ [] ghc ["--version"]
            putNormal "GHC"

        phony "submodules" buildSubmodules
        phony "cabal" $ readGhcPath >>= installCabal

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

readGhcPath :: Action GhcPath
readGhcPath = do
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

installCabal :: GhcPath -> Action ()
installCabal ghc = do
    execStack_ ["install", "cabal-install"]
    execCabal_ ["update"]
    execCabal_ ["install", "Cabal-2.4.1.0", "--with-compiler=" ++ ghc]

installHappy :: VersionNumber -> Action ()
installHappy versionNumber = execStackWithYaml versionNumber [ "install", "happy"]

buildHie :: FilePath -> VersionNumber -> Action ()
buildHie localBinDir versionNumber = do
    when (versionNumber `elem` ["hie-8.2.2", "hie-8.2.1"]) $
        execStackWithYaml versionNumber ["install", "happy"]
    execStackWithYaml versionNumber [ "build"]
    execStackWithYaml versionNumber [ "install"]
    cmd_
        "cp"
        [ localBinDir </> "hie" <.> exe]
        [ localBinDir </> "hie-" ++ versionNumber <.> exe
        ]

buildCopyCompilerTool :: VersionNumber -> Action () 
buildCopyCompilerTool versionNumber = do 
    execStackWithYaml versionNumber ["build", "--copy-compiler-tool"]

test :: VersionNumber -> Action () 
test versionNumber = execStackWithYaml versionNumber ["test"]

buildDoc :: VersionNumber -> Action ()
buildDoc versionNumber = do
    execStackWithYaml versionNumber ["install", "hoogle"]
    execStackWithYaml versionNumber ["exec", "hoogle", "generate"]

helpMessage :: Action ()
helpMessage = do 
    putNormal ""
    putNormal "Usage:"
    putNormal "    make <target>"
    putNormal ""
    putNormal "Targets:"
    putNormal "    build                Builds hie for all supported GHC versions (8.2.1, 8.2.2, 8.4.2 and 8.4.3, 8.4.4)"
    putNormal "    build-all            Builds hie and hoogle databases for all supported GHC versions"
    putNormal "    hie-8.2.1            Builds hie for GHC version 8.2.1 only"
    putNormal "    hie-8.2.2            Builds hie for GHC version 8.2.2 only"
    putNormal "    hie-8.4.2            Builds hie for GHC version 8.4.2 only"
    putNormal "    hie-8.4.3            Builds hie for GHC version 8.4.3 only"
    putNormal "    hie-8.4.4            Builds hie for GHC version 8.4.4 only"
    putNormal "    hie-8.6.1            Builds hie for GHC version 8.6.1 only"
    putNormal "    hie-8.6.2            Builds hie for GHC version 8.6.2 only"
    putNormal "    submodules           Updates local git submodules"
    putNormal "    cabal                NOTE 3: This is needed for stack only projects too"
    putNormal "    build-docs           Builds the Hoogle database for all supported GHC versions"
    putNormal "    test                 Runs hie tests"
    putNormal "    icu-macos-fix        Fixes icu related problems in MacOS"
    putNormal "    dist                 Creates a tarball containing all the hie binaries"
    putNormal "    help                 Show help"
    putNormal ""

execStackWithYaml :: VersionNumber -> [String] -> Action ()
execStackWithYaml versionNumber args = do 
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