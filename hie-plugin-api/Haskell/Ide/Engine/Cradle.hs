{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Haskell.Ide.Engine.Cradle (findLocalCradle) where

import           HIE.Bios as BIOS
import           HIE.Bios.Types
import           Haskell.Ide.Engine.MonadFunctions
import           Distribution.Helper
import           Distribution.Helper.Discover
import           Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty)
import           System.FilePath
import           System.Directory
import qualified Data.Map as M
import           Data.List (inits, sortOn, isPrefixOf)
import           Data.Maybe (listToMaybe)
import           Data.Ord
import           System.Exit

-- | Find the cradle that the given File belongs to.
--
-- First looks for a "hie.yaml" file in the directory of the file
-- or one of its parents. If this file is found, the cradle
-- is read from the config. If this config does not comply to the "hie.yaml"
-- specification, an error is raised.
--
-- If no "hie.yaml" can be found, the implicit config is used.
-- The implicit config uses different heuristics to determine the type
-- of the project that may or may not be accurate.
findLocalCradle :: FilePath -> IO Cradle
findLocalCradle fp = do
  -- Get the cabal directory from the cradle
  cradleConf <- BIOS.findCradle fp
  case cradleConf of
    Just yaml -> BIOS.loadCradle yaml
    Nothing   -> cabalHelperCradle fp

cabalHelperCradle :: FilePath -> IO Cradle
cabalHelperCradle file' = do
  -- TODO: recursive search
  root <- getCurrentDirectory
  file <- canonicalizePath file'
  logm $ "Cabal Helper dirs: " ++ show [root, file]
  projs <- findProjects root
  case projs of
    (Ex proj:_) -> do
      let actionNameSuffix = case proj of
            ProjLocV1CabalFile {} -> "Cabal-V1"
            ProjLocV1Dir {}       -> "Cabal-V1-Dir"
            ProjLocV2File {}      -> "Cabal-V2"
            ProjLocV2Dir {}       -> "Cabal-V2-Dir"
            ProjLocStackYaml {}   -> "Stack"
      let dist_dir = getDefaultDistDir proj
      env <- mkQueryEnv proj dist_dir
      packages <- runQuery projectPackages env
      -- Find the package the given file may belong to
      let realPackage = packages `findPackageFor` file
      -- Field `pSourceDir` often has the form `<cwd>/./plugin`
      -- but we only want `<cwd>/plugin`
      let normalisedPackageLocation = normalise $ pSourceDir realPackage
      -- Given the current directory: /projectRoot and the package is in
      -- /projectRoot/plugin, we only want ./plugin
      let relativePackageLocation = makeRelative root normalisedPackageLocation
      return
        Cradle { cradleRootDir = normalise (root </> relativePackageLocation)
               , cradleOptsProg = CradleAction { actionName = "Cabal-Helper-"
                                                   ++ actionNameSuffix
                                               , runCradle = cabalHelperAction
                                                   env
                                                   realPackage
                                                   normalisedPackageLocation
                                               }
               }
    -- TODO: fix this undefined, probably an errorIO
    _           -> undefined
  where
    cabalHelperAction :: QueryEnv v
                      -> Package v
                      -> FilePath
                      -> FilePath
                      -> IO (CradleLoadResult ComponentOptions)
    cabalHelperAction env package relativeDir fp = do
      let units = pUnits package
      -- Get all unit infos the given FilePath may belong to
      unitInfos_ <- mapM (\unit -> runQuery (unitInfo unit) env) units
      let fpRelativeDir = takeDirectory $ makeRelative relativeDir fp
      case getComponent fpRelativeDir unitInfos_ of
        Just comp -> do
          let fs = getFlags comp
          let targets = getTargets comp
          let ghcOptions = fs ++ targets
          debugm $ "Flags for \"" ++ fp ++ "\": " ++ show ghcOptions
          return
            $ CradleSuccess
              ComponentOptions { componentOptions = ghcOptions
                               , componentDependencies = []
                               }
        Nothing   -> return
          $ CradleFail
          $ CradleError (ExitFailure 2) ("Could not obtain flags for " ++ fp)

getComponent :: FilePath -> NonEmpty UnitInfo -> Maybe ChComponentInfo
getComponent dir ui = listToMaybe
  $ map snd
  $ filter (hasParent dir . fst)
  $ sortOn (Down . length . fst)
  $ concatMap (\ci -> map (, ci) (ciSourceDirs ci))
  $ concat
  $ M.elems . uiComponents <$> ui

getFlags :: ChComponentInfo -> [String]
getFlags = ciGhcOptions

getTargets :: ChComponentInfo -> [String]
getTargets comp = case ciEntrypoints comp of
  ChSetupEntrypoint {} -> []
  ChLibEntrypoint { chExposedModules, chOtherModules }
    -> map unChModuleName (chExposedModules ++ chOtherModules)
  ChExeEntrypoint { chMainIs, chOtherModules }
    -> chMainIs:map unChModuleName chOtherModules

hasParent :: FilePath -> FilePath -> Bool
hasParent child parent =
  any (equalFilePath parent) (map joinPath $ inits $ splitPath child)

findPackageFor :: NonEmpty (Package pt) -> FilePath -> Package pt
findPackageFor packages fp = packages
  & NonEmpty.filter (\p -> normalise (pSourceDir p) `isPrefixOf` fp)
  & sortOn (Down . length . pSourceDir)
  & head -- this head is unreasonable