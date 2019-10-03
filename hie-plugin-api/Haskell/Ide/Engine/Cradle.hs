{-# LANGUAGE TupleSections #-}
module Haskell.Ide.Engine.Cradle (findLocalCradle) where

import HIE.Bios as BIOS
import HIE.Bios.Types

import Haskell.Ide.Engine.MonadFunctions

import Distribution.Helper
import Distribution.Helper.Discover

import System.FilePath
import System.Directory

import qualified Data.Map as M
import Data.Foldable (toList)
import Data.List (inits, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord
import System.Exit

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
    Nothing -> cabalHelperCradle fp

cabalHelperCradle :: FilePath -> IO Cradle
cabalHelperCradle file' = do
  -- TODO find cradle
  root' <- getCurrentDirectory
  root <- canonicalizePath root'
  return Cradle
      { cradleRootDir = root
      , cradleOptsProg = CradleAction
          { actionName = "Cabal-Helper"
          , runCradle  = cabalHelperAction root
          }
      }

  where
    cabalHelperAction :: FilePath -> FilePath -> IO (CradleLoadResult ComponentOptions)
    cabalHelperAction root fp = do
      file <- canonicalizePath fp
      let file_dir = makeRelative root $ takeDirectory file
      debugm $ "Cabal Helper dirs: " ++ show [root, file, file_dir]
      projs <- findProjects root
      case projs of
        (Ex proj:_) -> do
          let [dist_dir] = findDistDirs proj
          env <- mkQueryEnv proj dist_dir
          units <- runQuery (allUnits id) env

          case getFlags file_dir $ toList units of
            Just fs -> do
              debugm $ "Flags for \"" ++ fp ++ "\": " ++ show fs
              return $ CradleSuccess
                        ComponentOptions
                          { componentOptions = fs ++ [file]
                          , componentDependencies = []
                          }

            Nothing -> return $ CradleFail $ CradleError (ExitFailure 2) ("Could not obtain flags for " ++ fp)
        _ -> return $ CradleFail $ CradleError (ExitFailure 1) ("Could not find project from: " ++ fp)

getFlags :: FilePath -> [UnitInfo] -> Maybe [String]
getFlags dir uis
  = listToMaybe
  $ map (ciGhcOptions . snd)
  $ filter (hasParent dir . fst)
  $ sortOn (Down . length . fst)
  $ concatMap (\ci -> map (,ci) (ciSourceDirs ci))
  $ concat
  $ M.elems . uiComponents <$> uis

hasParent :: FilePath -> FilePath -> Bool
hasParent child parent = any (equalFilePath parent) (map joinPath $ inits $ splitPath child)