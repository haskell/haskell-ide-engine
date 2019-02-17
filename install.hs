#!/usr/bin/env stack
{- stack
  --stack-yaml shake.yaml
  --install-ghc
  runghc
  --package shake
  --package directory
  --package tar
  --package zlib
-}

import qualified Data.ByteString.Lazy          as BS
import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Compression.GZip        as GZip

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Control.Monad
import           System.Directory               ( findExecutable )
import           System.Environment             ( getProgName
                                                , unsetEnv
                                                )
import           System.Info                    ( os
                                                , arch
                                                )

import           Data.List                      ( dropWhileEnd
                                                , intersperse
                                                )
import           Data.Char                      ( isSpace )

type VersionNumber = String
type GhcPath = String

-- |Defines all different hie versions that are buildable.
-- If they are edited, make sure to maintain the order of the versions.
hieVersions :: [VersionNumber]
hieVersions =
  ["8.2.1", "8.2.2", "8.4.2", "8.4.3", "8.4.4", "8.6.1", "8.6.2", "8.6.3"]

-- |Most recent version of hie.
-- Important for `dist`, the `hie-wrapper` of the most recent hie
-- will be copied to the tar-archive.
mostRecentHieVersion :: VersionNumber
mostRecentHieVersion = last hieVersions

main :: IO ()
main = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"
  shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    want ["help"]
    -- general purpose targets
    phony "submodules" updateSubmodules
    phony "cabal"      (getStackGhcPath mostRecentHieVersion >>= installCabal)
    phony "all"        helpMessage
    phony "help"       helpMessage
    phony "dist"       buildDist

    phony "cabal-ghcs" $ do
      ghcPaths <- findInstalledGhcs
      let
        msg =
          "Found the following GHC paths: \n"
            ++ unlines
                 (map (\(version, path) -> "ghc-" ++ version ++ ": " ++ path)
                      ghcPaths
                 )
      liftIO $ putStrLn $ embedInStars msg


    -- stack specific targets
    phony "build"      (need (reverse $ map ("hie-" ++) hieVersions))
    phony "build-all"  (need ["build"] >> need ["build-docs"])
    phony "build-docs" (need (reverse $ map ("build-doc-hie-" ++) hieVersions))
    phony "test"       (forM_ hieVersions stackTest)
    phony "build-copy-compiler-tool" $ forM_ hieVersions buildCopyCompilerTool
    forM_
      hieVersions
      (\version -> phony ("build-doc-hie-" ++ version) $ stackBuildDoc version)
    forM_
      hieVersions
      (\version -> phony ("hie-" ++ version) $ do
        need ["submodules"]
        need ["cabal"]
        stackBuildHie version
        stackInstallHie version
      )

    -- cabal specific targets
    phony "cabal-build" $ do
      ghcPaths <- findInstalledGhcs
      let ghcVersions = map fst ghcPaths
      need (reverse $ map ("cabal-hie-" ++) ghcVersions)

    phony "cabal-build-all" (need ["cabal-build"] >> need ["cabal-build-docs"])
    phony "cabal-build-docs" $ do
      ghcPaths <- findInstalledGhcs
      let ghcVersions = map fst ghcPaths
      need (reverse $ map ("cabal-build-doc-hie-" ++) ghcVersions)

    -- phony "cabal-test" (forM_ hieVersions stackTest)
    forM_
      hieVersions
      (\version -> phony ("cabal-build-doc-hie-" ++ version) $ do
        need ["submodules"]
        configureCabal version
        cabalBuildDoc version
      )
    forM_
      hieVersions
      (\version -> phony ("cabal-hie-" ++ version) $ do
        need ["submodules"]
        configureCabal version
        cabalBuildHie
        cabalInstallHie version
      )

    -- macos specific targets
    phony "icu-macos-fix"
          (need ["icu-macos-fix-install"] >> need ["icu-macos-fix-build"])
    phony "icu-macos-fix-install" (command_ [] "brew" ["install", "icu4c"])
    phony "icu-macos-fix-build" $ mapM_ buildIcuMacosFix hieVersions

-- |Creates a compressed tar-archive consisting of all hie versions and `hie-wrapper`.
-- Creates a temporary folder, copies all hie versions to it and compresses it in the end.
buildDist :: Action ()
buildDist = do
  need ["submodules"]
  need ["cabal"]
  -- Create the name of the resulting tar file.
  Stdout gitRef' <- command [] "git" ["describe", "--tags"]
  let gitRef      = trim gitRef'
  let hieDistName = concat ["hie-", gitRef, "-", arch, "-", os]
  -- define name constants for later use
  let hieWrapper  = "hie-wrapper" <.> exe
  let hie         = "hie" <.> exe
  let mkHie version = "hie-" ++ version <.> exe

  withTempDir
    (\temporaryDir -> do
      forM_ hieVersions $ \hieVersion -> do
        stackBuildHie hieVersion
        -- after building `hie` copy it to the temporary folder
        localInstallRoot <- getLocalInstallRoot hieVersion
        copyFile' (localInstallRoot </> "bin" </> hie)
                  (temporaryDir </> mkHie hieVersion)

        -- if the most recent hie-* version is copied,
        -- copy it again as the default hie version
        -- Also, add its hie-wrapper to the tar archive
        when (hieVersion == mostRecentHieVersion) $ do
          copyFile' (localInstallRoot </> "bin" </> hieWrapper)
                    (temporaryDir </> hieWrapper)
          copyFile' (localInstallRoot </> "bin" </> hie) (temporaryDir </> hie)

      -- After every hie has been built, pack them into a tar.
      -- Encrypt the resulting tar file with gzip
      liftIO
        $   BS.writeFile (hieDistName ++ ".tar.gz")
        .   GZip.compress
        .   Tar.write
        =<< Tar.pack temporaryDir (hieWrapper : hie : map mkHie hieVersions)
    )
  return ()

buildIcuMacosFix :: VersionNumber -> Action ()
buildIcuMacosFix version = execStackWithYaml_
  version
  [ "build"
  , "text-icu"
  , "--extra-lib-dirs=/usr/local/opt/icu4c/lib"
  , "--extra-include-dirs=/usr/local/opt/icu4c/include"
  ]

updateSubmodules :: Action ()
updateSubmodules = do
  command_ [] "git" ["submodule", "sync", "--recursive"]
  command_ [] "git" ["submodule", "update", "--init", "--recursive"]

configureCabal :: VersionNumber -> Action ()
configureCabal versionNumber = do
  ghcPath' <- getGhcPath versionNumber
  ghcPath  <- case ghcPath' of
    Nothing -> do
      liftIO $ putStrLn $ embedInStars (ghcVersionNotFound versionNumber)
      error (ghcVersionNotFound versionNumber)
    Just p -> return p
  execCabal_ ["new-configure", "-w", ghcPath, "--write-ghc-environment-files=never"]

findInstalledGhcs :: Action [(VersionNumber, GhcPath)]
findInstalledGhcs = foldM
  (\found version -> do
    path <- getGhcPath version
    case path of
      Nothing -> return found
      Just p  -> return $ (version, p) : found
  )
  []
  hieVersions

cabalBuildHie :: Action ()
cabalBuildHie = execCabal_ ["new-build", "--write-ghc-environment-files=never"]

cabalInstallHie :: VersionNumber -> Action ()
cabalInstallHie versionNumber = do
  execCabal_ ["new-install", "--write-ghc-environment-files=never", "exe:hie", "--overwrite-policy=always"]
  execCabal_ ["new-install", "--write-ghc-environment-files=never", "exe:hie", "--overwrite-policy=always", "--program-suffix=-" ++ versionNumber]

cabalBuildDoc :: VersionNumber -> Action ()
cabalBuildDoc versionNumber = do
  configureCabal versionNumber
  execCabal_ ["new-exec", "hoogle", "generate"]

cabalTest :: VersionNumber -> Action ()
cabalTest versionNumber = do
  configureCabal versionNumber
  execCabal_ ["new-test"]

installCabal :: GhcPath -> Action ()
installCabal ghc = do
  execStack_ ["install", "cabal-install"]
  execCabal_ ["update"]
  execCabal_ ["install", "Cabal-2.4.1.0", "--with-compiler=" ++ ghc]

stackBuildHie :: VersionNumber -> Action ()
stackBuildHie versionNumber = do
  execStackWithYaml_ versionNumber ["install", "happy"]
  execStackWithYaml_ versionNumber ["build"]
    `actionOnException` liftIO (putStrLn buildFailMsg)

buildFailMsg :: String
buildFailMsg =
  embedInStars
    $  "building failed, "
    ++ "try running `stack clean` and restart the build\n"
    ++ "if this does not work, open an issue at \n"
    ++ "https://github.com/haskell/haskell-ide-engine"

stackInstallHie :: VersionNumber -> Action ()
stackInstallHie versionNumber = do
  execStackWithYaml_ versionNumber ["install"]
  localBinDir      <- getLocalBin versionNumber
  localInstallRoot <- getLocalInstallRoot versionNumber
  let hie = "hie" <.> exe
  copyFile' (localInstallRoot </> "bin" </> hie)
            (localBinDir </> "hie-" ++ versionNumber <.> exe)
  copyFile' (localInstallRoot </> "bin" </> hie)
            (localBinDir </> "hie-" ++ dropExtension versionNumber <.> exe)

buildCopyCompilerTool :: VersionNumber -> Action ()
buildCopyCompilerTool versionNumber =
  execStackWithYaml_ versionNumber ["build", "--copy-compiler-tool"]

stackTest :: VersionNumber -> Action ()
stackTest versionNumber = execStackWithYaml_ versionNumber ["test"]

stackBuildDoc :: VersionNumber -> Action ()
stackBuildDoc versionNumber = do
  execStackWithYaml_ versionNumber ["install", "hoogle"]
  execStackWithYaml_ versionNumber ["exec", "hoogle", "generate"]

helpMessage :: Action ()
helpMessage = do
  scriptName <- liftIO getProgName
  out ""
  out "Usage:"
  out' ("stack " <> scriptName <> " <target>")
  out ""
  out "Targets:"
  mapM_ (out' . showTarget) targets
  out ""
 where
  out  = liftIO . putStrLn
  out' = out . ("    " ++)
  -- |Number of spaces the target name including whitespace should have.
  -- At least twenty, maybe more if target names are long. At most length of the longest target plus five.
  space :: Int
  space = maximum (20 : map ((+ 5) . length . fst) targets)

  -- |Show a target.
  -- Concatenates the target with its help message and inserts whitespace between them.
  showTarget :: (String, String) -> String
  showTarget (target, msg) =
    target ++ replicate (space - length target) ' ' ++ msg

  -- |Target for a specific ghc version
  stackHieTarget :: String -> (String, String)
  stackHieTarget version =
    ( "hie-" ++ version
    , "Builds hie for GHC version " ++ version ++ " only with stack"
    )
  -- |Target for a specific ghc version
  cabalHieTarget :: String -> (String, String)
  cabalHieTarget version =
    ( "cabal-hie-" ++ version
    , "Builds hie for GHC version " ++ version ++ " only with cabal new-build"
    )

  allVersionMessage :: String
  allVersionMessage =
    let msg         = intersperse ", " hieVersions
        lastVersion = last msg
    in  concat $ (init $ init msg) ++ [" and ", lastVersion]

  -- All targets the shake file supports
  targets = generalTargets ++ stackTargets ++ cabalTargets ++ macosTargets

  -- All targets with their respective help message.
  generalTargets =
    [ ("help" , "Show help")
    , ("cabal", "NOTE 3: This is needed for stack only projects too")
    , ("dist", "Creates a tarball containing all the hie binaries")
    ]

  macosTargets = [("icu-macos-fix", "Fixes icu related problems in MacOS")]

  stackTargets =
    [ ( "build"
      , "Builds hie for all supported GHC versions ("
      ++ allVersionMessage
      ++ ")"
      )
      , ( "build-all"
        , "Builds hie and hoogle databases for all supported GHC versions"
        )
      , ( "build-docs"
        , "Builds the Hoogle database for all supported GHC versions"
        )
      , ("test", "Runs hie tests with stack")
      ]
      ++ map stackHieTarget hieVersions

  cabalTargets =
    [ ( "cabal-ghcs"
      , "Show all GHC versions that can be installed via `cabal-build` and `cabal-build-all`."
      )
      , ( "cabal-build"
        , "Builds hie with cabal with all installed GHCs. Target `cabal-ghcs` shows all GHC versions that will be installed"
        )
      , ( "cabal-build-all"
        , "Builds hie and hoogle databases for all installed GHC versions with cabal"
        )
      , ( "cabal-build-docs"
        , "Builds the Hoogle database for all installed GHC versions with cabal"
        )
      , ("cabal-test", "Runs hie tests with cabal")
      ]
      ++ map cabalHieTarget hieVersions

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

-- |Get the path to the GHC compiler executable linked to the local `stack-$GHCVER.yaml`.
-- Equal to the command `stack path --stack-yaml $stack-yaml --compiler-exe`.
-- This might install a GHC if it is not already installed, thus, might fail if stack fails to install the GHC.
getStackGhcPath :: VersionNumber -> Action GhcPath
getStackGhcPath ghcVersion = do
  Stdout ghc <- execStackWithYaml ghcVersion ["path", "--compiler-exe"]
  return $ trim ghc

-- |Get the path to a GHC that has the version specified by `VersionNumber`
-- If no such GHC can be found, Nothing is returned.
getGhcPath :: VersionNumber -> Action (Maybe GhcPath)
getGhcPath ghcVersion = do
  pathMay <- liftIO $ findExecutable ("ghc-" ++ ghcVersion)
  case pathMay of
    Nothing -> do
      noPrefixPathMay <- liftIO $ findExecutable "ghc"
      case noPrefixPathMay of
        Nothing -> return Nothing
        Just p  -> do
          Stdout version <- command [] p ["--numeric-version"]
          if ghcVersion == trim version then return $ Just p else return Nothing
    p -> return p

-- |Read the local install root of the stack project specified by the VersionNumber
-- Returns the filepath of the local install root.
-- Equal to the command `stack path --local-install-root`
getLocalInstallRoot :: VersionNumber -> Action FilePath
getLocalInstallRoot hieVersion = do
  Stdout localInstallRoot' <- execStackWithYaml
    hieVersion
    ["path", "--local-install-root"]
  return $ trim localInstallRoot'

-- |Get the local binary path of stack.
-- Equal to the command `stack path --local-bin`
getLocalBin :: VersionNumber -> Action FilePath
getLocalBin versionNumber = do
  Stdout stackLocalDir' <- execStackWithYaml versionNumber
                                             ["path", "--local-bin"]
  return $ trim stackLocalDir'

-- |Trim the end of a string
trim :: String -> String
trim = dropWhileEnd isSpace

-- |Embed a string within two lines of stars to improve readability.
embedInStars :: String -> String
embedInStars str =
  let starsLine
        = "\n******************************************************************\n"
  in  starsLine <> str <> starsLine

-- |No suitable ghc version has been found.
ghcVersionNotFound :: VersionNumber -> String
ghcVersionNotFound versionNumber =
  "No GHC with version " <> versionNumber <> " has been found.\n"
  <> "Either install a fitting GHC, use the stack targets or modify the PATH variable accordingly."
