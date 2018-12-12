#!/usr/bin/env stack
{- stack 
  --stack-yaml=shake.yaml  
  --install-ghc runghc 
    --package shake 
    --package tar 
    --package zlib
-}

import qualified Data.ByteString.Lazy          as BS
import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Compression.GZip        as GZip

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Control.Monad
import           System.Environment
import           System.Info

type VersionNumber = String
type GhcPath = String

hieVersions :: [FilePath]
hieVersions = ["8.2.1", "8.2.2", "8.4.2", "8.4.3", "8.4.4", "8.6.1", "8.6.2"]

main :: IO ()
main = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"
  shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    phony "ghc" $ do
      ghc <- readGhcPath
      command_ [] ghc ["--version"]
      liftIO $ putStrLn "GHC"
    phony "submodules" updateSubmodules
    phony "cabal"      (readGhcPath >>= installCabal)
    phony "all"        helpMessage
    phony "help"       helpMessage
    phony "build"      (need (map ("hie-" ++) hieVersions))
    phony "build-all"  (need ["build"] >> need ["build-docs"])
    phony "dist"       buildDist
    phony "build-docs" (forM_ hieVersions buildDoc)
    phony "test"       (forM_ hieVersions test)
    phony "build-copy-compiler-tool" $ forM_ hieVersions buildCopyCompilerTool

    forM_
      hieVersions
      (\version -> phony ("hie-" ++ version) $ do
        need ["submodules"]
        need ["cabal"]
        stackLocalDir <- getLocalBin
        buildHie version
        installHie stackLocalDir version
      )

buildDist :: Action ()
buildDist = do
  Stdout gitRef' <- command [] "git" ["describe", "--tags"]
  let gitRef      = init gitRef'
  let hieDistName = concat ["hie-", gitRef, "-", arch, "-", os]
  withTempDir
    (\temporaryDir -> do
      forM_ hieVersions $ \hieVersion -> do
        buildHie hieVersion
        Stdout localInstallRoot' <- execStackWithYaml
          hieVersion
          ["path", "--local-install-root"]
        let localInstallRoot = init localInstallRoot'
        copyFile' (localInstallRoot </> "bin" </> "hie")
                  (temporaryDir </> "hie-" ++ hieVersion)

        -- if the most recent hie-* version is copied,
        -- copy it again as the default hie version
        -- Also, add the newest hie-wrapper to the tar archive 
        when (hieVersion == "8.6.2") $ do
          copyFile' (localInstallRoot </> "bin" </> "hie-wrapper")
                    (temporaryDir </> "hie-wrapper")
          copyFile' (localInstallRoot </> "bin" </> "hie")
                    (temporaryDir </> "hie")

      liftIO
        $   BS.writeFile (hieDistName ++ ".tar.gz")
        .   GZip.compress
        .   Tar.write
        =<< Tar.pack temporaryDir
                     ("hie-wrapper" : "hie" : map ("hie-" ++) hieVersions)
    )
  return ()

updateSubmodules :: Action ()
updateSubmodules = do
  command_ [] "git" ["submodule", "sync"]
  command_ [] "git" ["submodule", "update", "--init"]

installCabal :: GhcPath -> Action ()
installCabal ghc = do
  execStack_ ["install", "cabal-install"]
  execCabal_ ["v1-update"]
  execCabal_ ["v1-install", "Cabal-2.4.1.0", "--with-compiler=" ++ ghc]

installHappy :: VersionNumber -> Action ()
installHappy versionNumber =
  execStackWithYaml_ versionNumber ["install", "happy"]

buildHie :: VersionNumber -> Action ()
buildHie versionNumber = do
  when (versionNumber `elem` ["hie-8.2.2", "hie-8.2.1"])
    $ execStackWithYaml_ versionNumber ["install", "happy"]
  execStackWithYaml_ versionNumber ["build"]

installHie :: FilePath -> VersionNumber -> Action ()
installHie localBinDir versionNumber = do
  execStackWithYaml_ versionNumber ["install"]
  copyFile' (localBinDir </> "hie" <.> exe)
            (localBinDir </> "hie-" ++ versionNumber <.> exe)

buildCopyCompilerTool :: VersionNumber -> Action ()
buildCopyCompilerTool versionNumber =
  execStackWithYaml_ versionNumber ["build", "--copy-compiler-tool"]

test :: VersionNumber -> Action ()
test versionNumber = execStackWithYaml_ versionNumber ["test"]

buildDoc :: VersionNumber -> Action ()
buildDoc versionNumber = do
  execStackWithYaml_ versionNumber ["install", "hoogle"]
  execStackWithYaml_ versionNumber ["exec", "hoogle", "generate"]

helpMessage :: Action ()
helpMessage = do
  let out = liftIO . putStrLn
  out ""
  out "Usage:"
  out "    make <target>"
  out ""
  out "Targets:"
  out
    "    build                Builds hie for all supported GHC versions (8.2.1, 8.2.2, 8.4.2 and 8.4.3, 8.4.4)"
  out
    "    build-all            Builds hie and hoogle databases for all supported GHC versions"
  out "    hie-8.2.1            Builds hie for GHC version 8.2.1 only"
  out "    hie-8.2.2            Builds hie for GHC version 8.2.2 only"
  out "    hie-8.4.2            Builds hie for GHC version 8.4.2 only"
  out "    hie-8.4.3            Builds hie for GHC version 8.4.3 only"
  out "    hie-8.4.4            Builds hie for GHC version 8.4.4 only"
  out "    hie-8.6.1            Builds hie for GHC version 8.6.1 only"
  out "    hie-8.6.2            Builds hie for GHC version 8.6.2 only"
  out "    submodules           Updates local git submodules"
  out
    "    cabal                NOTE 3: This is needed for stack only projects too"
  out
    "    build-docs           Builds the Hoogle database for all supported GHC versions"
  out "    test                 Runs hie tests"
  out "    icu-macos-fix        Fixes icu related problems in MacOS"
  out
    "    dist                 Creates a tarball containing all the hie binaries"
  out "    help                 Show help"
  out ""

execStackWithYaml_ :: VersionNumber -> [String] -> Action ()
execStackWithYaml_ versionNumber args = do
  let stackFile = "stack-" ++ versionNumber ++ ".yaml"
  command_ [] "stack" (("--stack-yaml=" ++ stackFile) : args)

execStackWithYaml :: CmdResult r => VersionNumber -> [String] -> Action r
execStackWithYaml versionNumber args = do
  let stackFile = "stack-" ++ versionNumber ++ ".yaml"
  command [] "stack" (("--stack-yaml=" ++ stackFile) : args)

execStack :: CmdResult r => [String] -> Action r
execStack = command [] "stack"

execStack_ :: [String] -> Action ()
execStack_ = command_ [] "stack"

execCabal_ :: [String] -> Action ()
execCabal_ = command_ [] "cabal"

readGhcPath :: Action GhcPath
readGhcPath = do
  Stdout ghc' <- execStack ["path", "--compiler-exe"]
  return (init ghc')

getLocalBin :: Action FilePath
getLocalBin = do
  Stdout stackLocalDir' <- execStack ["path", "--local-bin"]
  return (init stackLocalDir')
