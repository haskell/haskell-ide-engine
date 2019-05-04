module Install where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Extra                      ( unlessM
                                                          , mapMaybeM
                                                          )
import           Data.Maybe                               ( isJust )
import           System.Directory                         ( listDirectory )
import           System.Environment                       ( getProgName
                                                          , unsetEnv
                                                          )
import           System.Info                              ( os
                                                          , arch
                                                          )

import           Data.Maybe                               ( isNothing
                                                          , mapMaybe
                                                          )
import           Data.List                                ( dropWhileEnd
                                                          , intersperse
                                                          , intercalate
                                                          , sort
                                                          , sortOn
                                                          )
import qualified Data.Text                     as T
import           Data.Char                                ( isSpace )
import           Data.Version                             ( parseVersion
                                                          , makeVersion
                                                          , showVersion
                                                          )
import           Data.Function                            ( (&) )
import           Text.ParserCombinators.ReadP             ( readP_to_S )

import           BuildSystem
import           Stack
import           Cabal
import           Version
import           Print
import           Env

-- | Most recent version of hie.
-- Shown in the more concise help message.
mostRecentHieVersion :: MonadIO m => m VersionNumber
mostRecentHieVersion = last <$> getHieVersions

defaultMain :: IO ()
defaultMain = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"

  ghcPaths <- findInstalledGhcs
  let ghcVersions = map fst ghcPaths

  hieVersions <- getHieVersions

  putStrLn $ "run from: " ++ buildSystem

  shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    want ["short-help"]
    -- general purpose targets
    phony "submodules"  updateSubmodules
    phony "cabal"       installCabal
    phony "short-help"  shortHelpMessage
    phony "all"         shortHelpMessage
    phony "help"        helpMessage
    phony "check-stack" checkStack
    phony "check-cabal" checkCabal
    -- TODO: check-cabal

    phony "cabal-ghcs" $ do
      let
        msg =
          "Found the following GHC paths: \n"
            ++ unlines
                 (map (\(version, path) -> "ghc-" ++ version ++ ": " ++ path)
                      ghcPaths
                 )
      printInStars msg

    -- default-targets
    phony "build" $ need [buildSystem ++ "-build"]
    phony "build-all" $ need [buildSystem ++ "-build-all"]
    phony "build-doc" $ need [buildSystem ++ "-build"]
    forM_
      hieVersions
      (\version ->
        phony ("hie-" ++ version) $ need [buildSystem ++ "-hie-" ++ version]
      )

    -- stack specific targets
    phony "stack-build"     (need (reverse $ map ("hie-" ++) hieVersions))
    phony "stack-build-all" (need ["build-doc", "build"])
    phony "stack-build-doc" $ do
      need ["submodules"]
      need ["check-stack"]
      stackBuildDoc
    forM_
      hieVersions
      (\version -> phony ("stack-hie-" ++ version) $ do
        need ["submodules"]
        need ["check-stack"]
        need ["cabal"]
        stackBuildHie version
        stackInstallHie version
      )

    -- cabal specific targets
    phony "cabal-build"     (need (map ("cabal-hie-" ++) ghcVersions))
    phony "cabal-build-all" (need ["cabal-build-doc", "cabal-build"])
    phony "cabal-build-doc" $ do
      need ["submodules"]
      need ["cabal"]
      cabalBuildDoc
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
buildIcuMacosFix version = execStackWithGhc_
  version
  [ "build"
  , "text-icu"
  , "--extra-lib-dirs=/usr/local/opt/icu4c/lib"
  , "--extra-include-dirs=/usr/local/opt/icu4c/include"
  ]

-- | update the submodules that the project is in the state as required by the `stack.yaml` files
updateSubmodules :: Action ()
updateSubmodules = do
  command_ [] "git" ["submodule", "sync"]
  command_ [] "git" ["submodule", "update", "--init"]

stackBuildHie :: VersionNumber -> Action ()
stackBuildHie versionNumber = execStackWithGhc_ versionNumber ["build"]
  `actionOnException` liftIO (putStrLn stackBuildFailMsg)

-- | copy the built binaries into the localBinDir
stackInstallHie :: VersionNumber -> Action ()
stackInstallHie versionNumber = do
  execStackWithGhc_ versionNumber ["install"]
  localBinDir      <- getLocalBin
  localInstallRoot <- getLocalInstallRoot versionNumber
  let hie = "hie" <.> exe
  copyFile' (localInstallRoot </> "bin" </> hie)
            (localBinDir </> "hie-" ++ versionNumber <.> exe)
  copyFile' (localInstallRoot </> "bin" </> hie)
            (localBinDir </> "hie-" ++ dropExtension versionNumber <.> exe)

buildCopyCompilerTool :: VersionNumber -> Action ()
buildCopyCompilerTool versionNumber =
  execStackWithGhc_ versionNumber ["build", "--copy-compiler-tool"]

stackBuildDoc :: Action ()
stackBuildDoc = do
  execStackShake_ ["build", "hoogle"]
  execStackShake_ ["exec", "hoogle", "generate"]

-- | short help message is printed by default
shortHelpMessage :: Action ()
shortHelpMessage = do
  hieVersions <- getHieVersions
  let out = liftIO . putStrLn
  scriptName <- liftIO getProgName
  out ""
  out "Usage:"
  out' ("stack " <> scriptName <> " <target>")
  out ""
  out "Targets:"
  mapM_ (out' . showTarget (spaces hieVersions)) (targets hieVersions)
  out ""
 where
  out  = liftIO . putStrLn
  out' = out . ("    " ++)

  spaces hieVersions = space (targets hieVersions)
  targets hieVersions =
    [ ("help", "Show help message including all targets")
    , emptyTarget
    , ( "build"
      , "Builds hie for all supported GHC versions ("
        ++ allVersionMessage hieVersions
        ++ ")"
      )
    , stackBuildAllTarget
    -- , stackHieTarget mostRecentHieVersion
    , stackBuildDocTarget
    , stackHieTarget (last hieVersions)
    , emptyTarget
    , ( "cabal-ghcs"
      , "Show all GHC versions that can be installed via `cabal-build` and `cabal-build-all`."
      )
    , cabalBuildTarget
    , cabalBuildAllTarget
    -- , cabalHieTarget mostRecentHieVersion
    , cabalBuildDocTarget
    , cabalHieTarget (last hieVersions)
    ]


helpMessage :: Action ()
helpMessage = do

  hieVersions <- getHieVersions
  scriptName  <- liftIO getProgName
  out ""
  out "Usage:"
  out' ("stack " <> scriptName <> " <target>")
  out ""
  out "Targets:"
  mapM_ (out' . showTarget (spaces hieVersions)) (targets hieVersions)
  out ""
 where
  out  = liftIO . putStrLn
  out' = out . ("    " ++)

  spaces hieVersions = space (targets hieVersions)
  -- All targets the shake file supports
  targets :: [VersionNumber] -> [(String, String)]
  targets hieVersions = intercalate
    [emptyTarget]
    [ generalTargets
    , stackTargets hieVersions
    , cabalTargets hieVersions
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

  stackTargets hieVersions =
    [ ( "build"
      , "Builds hie for all supported GHC versions ("
      ++ allVersionMessage hieVersions
      ++ ")"
      )
      , stackBuildAllTarget
      , stackBuildDocTarget
      , ("test", "Runs hie tests with stack")
      ]
      ++ map stackHieTarget hieVersions

  cabalTargets hieVersions =
    [ ( "cabal-ghcs"
      , "Show all GHC versions that can be installed via `cabal-build` and `cabal-build-all`."
      )
      , cabalBuildTarget
      , cabalBuildAllTarget
      , cabalBuildDocTarget
      , ("cabal-test", "Runs hie tests with cabal")
      ]
      ++ map cabalHieTarget hieVersions

-- | Empty target. Purpose is to introduce a newline between the targets
emptyTarget :: (String, String)
emptyTarget = ("", "")

-- | Target for a specific ghc version
stackHieTarget :: String -> TargetDescription
stackHieTarget version =
  ( "hie-" ++ version
  , "Builds hie for GHC version " ++ version ++ " only with stack"
  )

-- | Target for a specific ghc version
cabalHieTarget :: String -> TargetDescription
cabalHieTarget version =
  ( "cabal-hie-" ++ version
  , "Builds hie for GHC version " ++ version ++ " only with cabal new-build"
  )

stackBuildDocTarget :: TargetDescription
stackBuildDocTarget = ("build-doc", "Builds the Hoogle database")

stackBuildAllTarget :: TargetDescription
stackBuildAllTarget =
  ( "build-all"
  , "Builds hie for all supported GHC versions and the hoogle database"
  )

cabalBuildTarget :: TargetDescription
cabalBuildTarget =
  ("cabal-build", "Builds hie with cabal with all installed GHCs.")

cabalBuildDocTarget :: TargetDescription
cabalBuildDocTarget =
  ("cabal-build-doc", "Builds the Hoogle database with cabal")

cabalBuildAllTarget :: TargetDescription
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
    in  concat $ init (init msg) ++ [" and ", lastVersion]


-- | Get the path to the GHC compiler executable linked to the local `stack-$GHCVER.yaml`.
-- Equal to the command `stack path --stack-yaml $stack-yaml --compiler-exe`.
-- This might install a GHC if it is not already installed, thus, might fail if stack fails to install the GHC.
getStackGhcPath :: VersionNumber -> Action GhcPath
getStackGhcPath ghcVersion = do
  Stdout ghc <- execStackWithGhc ghcVersion ["path", "--compiler-exe"]
  return $ trim ghc

-- | Read the local install root of the stack project specified by the VersionNumber
-- Returns the filepath of the local install root.
-- Equal to the command `stack path --local-install-root`
getLocalInstallRoot :: VersionNumber -> Action FilePath
getLocalInstallRoot hieVersion = do
  Stdout localInstallRoot' <- execStackWithGhc
    hieVersion
    ["path", "--local-install-root"]
  return $ trim localInstallRoot'

-- |Stack build fails message
stackBuildFailMsg :: String
stackBuildFailMsg =
  embedInStars
    $  "Building failed, "
    ++ "Try running `stack clean` and restart the build\n"
    ++ "If this does not work, open an issue at \n"
    ++ "\thttps://github.com/haskell/haskell-ide-engine"
