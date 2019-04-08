#!/usr/bin/env stack
{- stack
  script
  --resolver nightly-2018-12-15
  --package shake
  --package directory
  --package extra
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Extra (unlessM, mapMaybeM)
import           Data.Maybe (isJust)
import           System.Directory               ( findExecutable )
import           System.Environment             ( getProgName
                                                , unsetEnv
                                                )
import           System.Info                    ( os
                                                , arch
                                                )

import           Data.List                      ( dropWhileEnd
                                                , intersperse
                                                , intercalate
                                                )
import           Data.Char                      ( isSpace )

type VersionNumber = String
type GhcPath = String

-- |Defines all different hie versions that are buildable.
-- If they are edited, make sure to maintain the order of the versions.
hieVersions :: [VersionNumber]
hieVersions =
  [ "8.2.1"
  , "8.2.2"
  , "8.4.2"
  , "8.4.3"
  , "8.4.4"
  , "8.6.1"
  , "8.6.2"
  , "8.6.3"
  , "8.6.4"
  ]

-- |Most recent version of hie.
-- Shown in the more concise help message.
mostRecentHieVersion :: VersionNumber
mostRecentHieVersion = last hieVersions

main :: IO ()
main = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"

  ghcPaths <- findInstalledGhcs
  let ghcVersions = map fst ghcPaths

  shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    want ["short-help"]
    -- general purpose targets
    phony "submodules" updateSubmodules
    phony "cabal"      installCabal
    phony "short-help" shortHelpMessage
    phony "all"        shortHelpMessage
    phony "help"       helpMessage

    phony "cabal-ghcs" $ do
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
    phony "build-all"  (need ["build-doc", "build"])
    phony "test" $ do
      need ["submodules"]
      need ["cabal"]
      forM_ hieVersions stackTest

    phony "build-copy-compiler-tool" $ forM_ hieVersions buildCopyCompilerTool

    phony "build-doc" $ do
        need ["submodules"]
        stackBuildDoc

    -- main targets for building hie with `stack`
    forM_
      hieVersions
      (\version -> phony ("hie-" ++ version) $ do
        need ["submodules"]
        need ["cabal"]
        stackBuildHie version
        stackInstallHie version
      )

    -- cabal specific targets
    phony "cabal-build"      (need (map ("cabal-hie-" ++) ghcVersions))
    phony "cabal-build-all"  (need ["cabal-build-doc", "cabal-build"])
    phony "cabal-build-doc" $ do
        need ["submodules"]
        need ["cabal"]
        cabalBuildDoc

    phony "cabal-test" $ do
      need ["submodules"]
      need ["cabal"]
      forM_ ghcVersions cabalTest

    forM_
      hieVersions
      (\version -> phony ("cabal-hie-" ++ version) $ do
        validateCabalNewInstallIsSupported
        need ["submodules"]
        need ["cabal"]
        cabalBuildHie version
        cabalInstallHie version
      )

    -- macos specific targets
    phony "icu-macos-fix"
          (need ["icu-macos-fix-install"] >> need ["icu-macos-fix-build"])
    phony "icu-macos-fix-install" (command_ [] "brew" ["install", "icu4c"])
    phony "icu-macos-fix-build" $ mapM_ buildIcuMacosFix hieVersions


buildIcuMacosFix :: VersionNumber -> Action ()
buildIcuMacosFix version = execStackWithYaml_
  version
  [ "build"
  , "text-icu"
  , "--extra-lib-dirs=/usr/local/opt/icu4c/lib"
  , "--extra-include-dirs=/usr/local/opt/icu4c/include"
  ]

-- |update the submodules that the project is in the state as required by the `stack.yaml` files
updateSubmodules :: Action ()
updateSubmodules = do
  command_ [] "git" ["submodule", "sync", "--recursive"]
  command_ [] "git" ["submodule", "update", "--init", "--recursive"]

validateCabalNewInstallIsSupported :: Action ()
validateCabalNewInstallIsSupported = when (os `elem` ["mingw32", "win32"]) $ do
  liftIO $ putStrLn $ embedInStars cabalInstallNotSuported
  error cabalInstallNotSuported

configureCabal :: VersionNumber -> Action ()
configureCabal versionNumber = do
  ghcPath <- getGhcPath versionNumber >>= \case
    Nothing -> do -- TODO: this is better written using a monad-transformer
      liftIO $ putStrLn $ embedInStars (ghcVersionNotFound versionNumber)
      error (ghcVersionNotFound versionNumber)
    Just p -> return p
  execCabal_
    ["new-configure", "-w", ghcPath, "--write-ghc-environment-files=never"]

findInstalledGhcs :: IO [(VersionNumber, GhcPath)]
findInstalledGhcs = mapMaybeM
  (\version -> do
    getGhcPath version >>= \case
      Nothing -> return Nothing
      Just p  -> return $ Just (version, p)
  )
  hieVersions

cabalBuildHie :: VersionNumber -> Action ()
cabalBuildHie versionNumber = do
  configureCabal versionNumber
  execCabal_ ["new-build", "--write-ghc-environment-files=never"]

cabalInstallHie :: VersionNumber -> Action ()
cabalInstallHie versionNumber = do
  localBin <- getLocalBin
  execCabal_
    [ "new-install"
    , "--write-ghc-environment-files=never"
    , "--symlink-bindir=" ++ localBin
    , "exe:hie"
    , "--overwrite-policy=always"
    ]
  copyFile' (localBin </> "hie" <.> exe)
            (localBin </> "hie-" ++ versionNumber <.> exe)
  copyFile' (localBin </> "hie" <.> exe)
            (localBin </> "hie-" ++ dropExtension versionNumber <.> exe)

cabalBuildDoc :: Action ()
cabalBuildDoc = do
  execCabal_ ["new-build", "hoogle", "generate"]
  execCabal_ ["new-exec", "hoogle", "generate"]

cabalTest :: VersionNumber -> Action ()
cabalTest versionNumber = do
  configureCabal versionNumber
  execCabal_ ["new-test"]

installCabal :: Action ()
installCabal = do
  -- install `cabal-install` if not already installed
  unlessM (existsExecutable "cabal") $ do
    execStack_ ["install", "--stack-yaml=shake.yaml", "cabal-install"]
  execCabal_ ["update"]
  ghc <- getStackGhcPath mostRecentHieVersion
  execCabal_ ["install", "Cabal-2.4.1.0", "--with-compiler=" ++ ghc]

stackBuildHie :: VersionNumber -> Action ()
stackBuildHie versionNumber = do
  execStackWithYaml_ versionNumber ["build"]
    `actionOnException` liftIO (putStrLn stackBuildFailMsg)

-- | copy the built binaries into the localBinDir
stackInstallHie :: VersionNumber -> Action ()
stackInstallHie versionNumber = do
  execStackWithYaml_ versionNumber ["install"]
  localBinDir      <- getLocalBin
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

stackBuildDoc :: Action ()
stackBuildDoc = do
  execStack_ ["--stack-yaml=shake.yaml", "build", "hoogle"]
  execStack_ ["--stack-yaml=shake.yaml", "exec", "hoogle", "generate"]

-- | short help message is printed by default
shortHelpMessage :: Action ()
shortHelpMessage = do
  let out = liftIO . putStrLn
  scriptName <- liftIO getProgName
  out ""
  out "Usage:"
  out' ("stack " <> scriptName <> " <target>")
  out ""
  out "Targets:"
  mapM_ (out' . showTarget spaces) targets
  out ""
 where
  out    = liftIO . putStrLn
  out'   = out . ("    " ++)

  spaces = space targets
  targets =
    [ ("help", "Show help message including all targets")
    , emptyTarget
    , ( "build"
      , "Builds hie for all supported GHC versions ("
        ++ allVersionMessage hieVersions
        ++ ")"
      )
    , stackBuildAllTarget
    , stackHieTarget mostRecentHieVersion
    , stackBuildDocTarget
    , stackHieTarget "8.4.4"
    , emptyTarget
    , ( "cabal-ghcs"
      , "Show all GHC versions that can be installed via `cabal-build` and `cabal-build-all`."
      )
    , cabalBuildTarget
    , cabalBuildAllTarget
    , cabalHieTarget mostRecentHieVersion
    , cabalBuildDocTarget
    , cabalHieTarget "8.4.4"
    ]


helpMessage :: Action ()
helpMessage = do
  scriptName <- liftIO getProgName
  out ""
  out "Usage:"
  out' ("stack " <> scriptName <> " <target>")
  out ""
  out "Targets:"
  mapM_ (out' . showTarget spaces) targets
  out ""
 where
  out    = liftIO . putStrLn
  out'   = out . ("    " ++)

  spaces = space targets
  -- All targets the shake file supports
  targets :: [(String, String)]
  targets =
    intercalate
      [emptyTarget]
      [ generalTargets
      , stackTargets
      , cabalTargets
      , macosTargets
      ]

  -- All targets with their respective help message.
  generalTargets =
    [ ("help", "Show help message including all targets")
    , ( "cabal"
      , "Makes sure that Cabal the lib is available for cabal-helper-wapper, to speed up project start"
      )
    ]

  macosTargets = [("icu-macos-fix", "Fixes icu related problems in MacOS")]

  stackTargets =
    [ ( "build"
      , "Builds hie for all supported GHC versions ("
      ++ allVersionMessage hieVersions
      ++ ")"
      )
      , stackBuildAllTarget
      , stackBuildDocTarget
      , ("test", "Runs hie tests with stack")
      ]
      ++ map stackHieTarget      hieVersions

  cabalTargets =
    [ ( "cabal-ghcs"
      , "Show all GHC versions that can be installed via `cabal-build` and `cabal-build-all`."
      )
      , cabalBuildTarget
      , cabalBuildAllTarget
      , cabalBuildDocTarget
      , ("cabal-test", "Runs hie tests with cabal")
      ]
      ++ map cabalHieTarget      hieVersions

-- | Empty target. Purpose is to introduce a newline between the targets
emptyTarget :: (String, String)
emptyTarget = ("", "")

-- |Number of spaces the target name including whitespace should have.
-- At least twenty, maybe more if target names are long and at least the length of the longest target plus five.
space :: [(String, String)] -> Int
space phonyTargets = maximum (20 : map ((+ 5) . length . fst) phonyTargets)

-- |Show a target.
-- Concatenates the target with its help message and inserts whitespace between them.
showTarget :: Int -> (String, String) -> String
showTarget spaces (target, msg) =
  target ++ replicate (spaces - length target) ' ' ++ msg

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

stackBuildDocTarget :: (String, String)
stackBuildDocTarget =
  ( "build-doc"
  , "Builds the Hoogle database"
  )

stackBuildAllTarget :: (String, String)
stackBuildAllTarget =
  ( "build-all"
  , "Builds hie for all supported GHC versions and the hoogle database"
  )

cabalBuildTarget :: (String, String)
cabalBuildTarget =
  ("cabal-build", "Builds hie with cabal with all installed GHCs.")

cabalBuildDocTarget :: (String, String)
cabalBuildDocTarget =
  ( "cabal-build-doc"
  , "Builds the Hoogle database with cabal"
  )

cabalBuildAllTarget :: (String, String)
cabalBuildAllTarget =
  ( "cabal-build-all"
  , "Builds hie for all installed GHC versions and the hoogle database with cabal"
  )

-- | Creates a message of the form "a, b, c and d", where a,b,c,d are GHC versions.
-- If there is no GHC in the list of `hieVersions`
allVersionMessage :: [String] -> String
allVersionMessage wordList = case wordList of
  []  -> ""
  [a] -> show a
  (a : as) ->
    let msg         = intersperse ", " wordList
        lastVersion = last msg
    in  concat $ (init $ init msg) ++ [" and ", lastVersion]


-- TODO: more sophisticated interface to stack and cabal

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

existsExecutable :: MonadIO m => String -> m Bool
existsExecutable executable = liftIO $ isJust <$> findExecutable executable

-- |Get the path to the GHC compiler executable linked to the local `stack-$GHCVER.yaml`.
-- Equal to the command `stack path --stack-yaml $stack-yaml --compiler-exe`.
-- This might install a GHC if it is not already installed, thus, might fail if stack fails to install the GHC.
getStackGhcPath :: VersionNumber -> Action GhcPath
getStackGhcPath ghcVersion = do
  Stdout ghc <- execStackWithYaml ghcVersion ["path", "--compiler-exe"]
  return $ trim ghc

-- |Get the path to a GHC that has the version specified by `VersionNumber`
-- If no such GHC can be found, Nothing is returned.
-- First, it is checked whether there is a GHC with the name `ghc-$VersionNumber`.
-- If this yields no result, it is checked, whether the numeric-version of the `ghc`
-- command fits to the desired version.
getGhcPath :: MonadIO m => VersionNumber -> m (Maybe GhcPath)
getGhcPath ghcVersion = liftIO $ do
  findExecutable ("ghc-" ++ ghcVersion) >>= \case
    Nothing -> do
      findExecutable "ghc" >>= \case
        Nothing -> return Nothing
        Just p  -> do
          Stdout version <- cmd p ["--numeric-version"] :: IO (Stdout String)
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
getLocalBin :: Action FilePath
getLocalBin = do
  Stdout stackLocalDir' <- execStack
    ["path", "--stack-yaml=shake.yaml", "--local-bin"]
  return $ trim stackLocalDir'

-- |Trim the end of a string
trim :: String -> String
trim = dropWhileEnd isSpace

-- |Embed a string within two lines of stars to improve perceivability and, thus, readability.
embedInStars :: String -> String
embedInStars str =
  let starsLine
        = "\n******************************************************************\n"
  in  starsLine <> str <> starsLine

-- |Stack build fails message
stackBuildFailMsg :: String
stackBuildFailMsg =
  embedInStars
    $  "Building failed, "
    ++ "Try running `stack clean` and restart the build\n"
    ++ "If this does not work, open an issue at \n"
    ++ "\thttps://github.com/haskell/haskell-ide-engine"

-- |No suitable ghc version has been found. Show a message.
ghcVersionNotFound :: VersionNumber -> String
ghcVersionNotFound versionNumber =
  "No GHC with version "
    <> versionNumber
    <> " has been found.\n"
    <> "Either install a fitting GHC, use the stack targets or modify the PATH variable accordingly."

-- | Error message when a windows system tries to install HIE via `cabal new-install`
cabalInstallNotSuported :: String
cabalInstallNotSuported =
  "This system has been identified as a windows system.\n"
    ++ "Unfortunately, `cabal new-install` is currently not supported on windows.\n"
    ++ "Please use one of the stack-based targets.\n\n"
    ++ "If this system has been falsely identified, please open an issue at:\n\thttps://github.com/haskell/haskell-ide-engine\n"
