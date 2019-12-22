{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Information and display strings for HIE's version
-- and the current project's version
module Haskell.Ide.Engine.Version where

import           Control.Exception
import           Data.Maybe
import           Development.GitRev              (gitCommitCount)
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Options.Applicative.Simple      (simpleVersion)
import           Haskell.Ide.Engine.Cradle (isStackCradle)
import qualified HIE.Bios.Types as BIOS
import qualified Paths_haskell_ide_engine        as Meta
import qualified System.Log.Logger as L
import qualified Data.Text                       as T
import qualified Data.Versions                   as V
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

-- ---------------------------------------------------------------------

hieGhcDisplayVersion :: String
hieGhcDisplayVersion = compilerName ++ "-" ++ VERSION_ghc

getProjectGhcVersion :: BIOS.Cradle -> IO String
getProjectGhcVersion crdl = do
  isStackInstalled <- isJust <$> findExecutable "stack"
  if isStackCradle crdl && isStackInstalled
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


tryCommand :: String -> IO String
tryCommand cmd =
  init <$> readCreateProcess (shell cmd) ""

hieGhcVersion :: String
hieGhcVersion = VERSION_ghc

-- ---------------------------------------------------------------------

getStackVersion :: IO (Maybe V.Version)
getStackVersion = do
  isStackInstalled   <- isJust <$> findExecutable "stack"
  if isStackInstalled
    then do
      versionStr <- tryCommand "stack --numeric-version"
      case V.version (T.pack versionStr) of
        Left _err -> return Nothing
        Right v -> return (Just v)
    else return Nothing

stack193Version :: V.Version
stack193Version = case V.version "1.9.3" of
  Left err -> error $ "stack193Version:err=" ++ show err
  Right v -> v

-- ---------------------------------------------------------------------

checkCabalInstall :: IO Bool
checkCabalInstall = isJust <$> findExecutable "cabal"

-- ---------------------------------------------------------------------
