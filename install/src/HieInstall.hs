module HieInstall where

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
import           System.Environment                       ( unsetEnv )
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
import           Help

defaultMain :: IO ()
defaultMain = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"

  -- used for cabal-based targets
  ghcPaths <- findInstalledGhcs
  let ghcVersions = map fst ghcPaths

  -- used for stack-based targets
  hieVersions <- getHieVersions

  let versions = BuildableVersions { stackVersions = hieVersions
                                   , cabalVersions = ghcVersions
                                   }

  putStrLn $ "run from: " ++ buildSystem

  shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    want ["short-help"]
    -- general purpose targets
    phony "submodules"  updateSubmodules
    phony "cabal"       installCabalWithStack
    phony "short-help"  shortHelpMessage
    phony "all"         shortHelpMessage
    phony "help"        (helpMessage versions)
    phony "check-stack" checkStack
    phony "check-cabal" checkCabal

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
    phony "build-data" $ need [buildSystem ++ "-build-data"]
    forM_
      (getDefaultBuildSystemVersions versions)
      (\version ->
        phony ("hie-" ++ version) $ need [buildSystem ++ "-hie-" ++ version]
      )

    -- stack specific targets
    when isRunFromStack (phony "stack-install-cabal" (need ["cabal"]))
    phony "stack-build"     (need (reverse $ map ("stack-hie-" ++) hieVersions))
    phony "stack-build-all" (need ["build-data", "build"])
    phony "stack-build-data" $ do
      need ["submodules"]
      need ["check-stack"]
      stackBuildData
    forM_
      hieVersions
      (\version -> phony ("stack-hie-" ++ version) $ do
        need ["submodules"]
        need ["check-stack"]
        stackBuildHie version
        stackInstallHie version
      )

    -- cabal specific targets
    phony "cabal-build"     (need (map ("cabal-hie-" ++) ghcVersions))
    phony "cabal-build-all" (need ["cabal-build-data", "cabal-build"])
    phony "cabal-build-data" $ do
      need ["submodules"]
      need ["cabal"]
      cabalBuildData
    forM_
      ghcVersions
      (\version -> phony ("cabal-hie-" ++ version) $ do
        need ["submodules"]
        need ["cabal"]
        validateCabalNewInstallIsSupported
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
