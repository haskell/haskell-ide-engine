module Env where

import           Development.Shake
import           Development.Shake.Command
import           Control.Monad.IO.Class
import           Control.Monad
import           Development.Shake.FilePath
import           System.Info                              ( os
                                                          , arch
                                                          )
import           Data.Maybe                               ( isJust )
import           System.Directory                         ( findExecutable
                                                          , listDirectory
                                                          )
import           Data.Function                            ( (&) )
import           Data.List                                ( sort )
import           Control.Monad.Extra                      ( mapMaybeM )
import           Data.Maybe                               ( isNothing
                                                          , mapMaybe
                                                          )
import qualified Data.Text                     as T

import           Version
import           Print


type GhcPath = String

existsExecutable :: MonadIO m => String -> m Bool
existsExecutable executable = liftIO $ isJust <$> findExecutable executable


-- | Check if the current system is windows
isWindowsSystem :: Bool
isWindowsSystem = os `elem` ["mingw32", "win32"]

findInstalledGhcs :: IO [(VersionNumber, GhcPath)]
findInstalledGhcs = do
  hieVersions <- getHieVersions :: IO [VersionNumber]
  mapMaybeM
    (\version -> getGhcPath version >>= \case
      Nothing -> return Nothing
      Just p  -> return $ Just (version, p)
    )
    (reverse hieVersions)

-- | Get the path to a GHC that has the version specified by `VersionNumber`
-- If no such GHC can be found, Nothing is returned.
-- First, it is checked whether there is a GHC with the name `ghc-$VersionNumber`.
-- If this yields no result, it is checked, whether the numeric-version of the `ghc`
-- command fits to the desired version.
getGhcPath :: MonadIO m => VersionNumber -> m (Maybe GhcPath)
getGhcPath ghcVersion =
  liftIO $ findExecutable ("ghc-" ++ ghcVersion) >>= \case
    Nothing -> do
      findExecutable "ghc" >>= \case
        Nothing -> return Nothing
        Just p  -> do
          Stdout version <- cmd p ["--numeric-version"] :: IO (Stdout String)
          if ghcVersion == trim version then return $ Just p else return Nothing
    p -> return p


-- | No suitable ghc version has been found. Show a message.
ghcVersionNotFoundFailMsg :: VersionNumber -> String
ghcVersionNotFoundFailMsg versionNumber =
  "No GHC with version "
    <> versionNumber
    <> " has been found.\n"
    <> "Either install a fitting GHC, use the stack targets or modify the PATH variable accordingly."


-- | Defines all different hie versions that are buildable.
--
-- The current directory is scanned for `stack-*.yaml` files.
-- On windows, `8.6.3` is excluded as this version of ghc does not work there
getHieVersions :: MonadIO m => m [VersionNumber]
getHieVersions = do
  let stackYamlPrefix = T.pack "stack-"
  let stackYamlSuffix = T.pack ".yaml"
  files <- liftIO $ listDirectory "."
  let hieVersions =
        files
          & map T.pack
          & mapMaybe
              (T.stripPrefix stackYamlPrefix >=> T.stripSuffix stackYamlSuffix)
          & map T.unpack
        -- the following line excludes `8.6.3` on windows systems
          & filter (\p -> not isWindowsSystem || p /= "8.6.3")
          & sort
  return hieVersions


-- | Most recent version of hie.
-- Shown in the more concise help message.
mostRecentHieVersion :: MonadIO m => m VersionNumber
mostRecentHieVersion = last <$> getHieVersions
