{-# LANGUAGE CPP, TemplateHaskell #-}
-- | Information and display strings for HIE's version
-- and the current project's version
module Haskell.Ide.Engine.Version where

import           Control.Exception
import           Data.Maybe
import           Development.GitRev              (gitCommitCount)
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Options.Applicative.Simple      (simpleVersion)
import qualified Paths_haskell_ide_engine        as Meta
import qualified System.Log.Logger as L
import           System.Directory
import           System.Info
import           System.Process

hieVersion :: String
hieVersion =
  let commitCount = $gitCommitCount
  in concat $ concat
    [ [$(simpleVersion Meta.version)]
      -- Leave out number of commits for --depth=1 clone
      -- See https://github.com/commercialhaskell/stack/issues/792
    , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                            commitCount /= ("UNKNOWN" :: String)]
    , [" ", display buildArch]
    , [" ", hieGhcDisplayVersion]
    ]

hieGhcVersion :: String
hieGhcVersion = VERSION_ghc

hieGhcDisplayVersion :: String
hieGhcDisplayVersion = compilerName ++ "-" ++ hieGhcVersion

-- TODO: Replace with cabal-helper query
getProjectGhcVersion :: IO String
getProjectGhcVersion = do
  isStackProject   <- doesFileExist "stack.yaml"
  isStackInstalled <- isJust <$> findExecutable "stack"
  if isStackProject && isStackInstalled
    then do
      L.infoM "hie" "Using stack GHC version"
      catch (tryCommand "stack ghc -- --numeric-version") $ \e -> do
        L.errorM "hie" $ show (e :: SomeException)
        L.infoM "hie" "Couldn't find stack version, falling back to plain GHC"
        getGhcVersion
    else do
      L.infoM "hie" "Using plain GHC version"
      getGhcVersion

  where
    getGhcVersion = do
      isGhcInstalled   <- isJust <$> findExecutable "ghc"
      if isGhcInstalled
        then tryCommand "ghc --numeric-version"
        else return "No System GHC found"

    tryCommand cmd =
      init <$> readCreateProcess (shell cmd) ""