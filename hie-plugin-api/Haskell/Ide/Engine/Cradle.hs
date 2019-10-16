{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Haskell.Ide.Engine.Cradle (findLocalCradle, isStackCradle) where

import           HIE.Bios as BIOS
import           HIE.Bios.Types as BIOS
import           Haskell.Ide.Engine.MonadFunctions
import           Distribution.Helper
import           Distribution.Helper.Discover
import           Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty)
import           System.FilePath
import           System.Directory
import qualified Data.Map as M
import           Data.List (inits, sortOn, isPrefixOf, find)
import           Data.Maybe (listToMaybe)
import           Data.Ord (Down(..))
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
    Just yaml -> fixCradle <$> BIOS.loadCradle yaml

    Nothing   -> cabalHelperCradle fp

-- | Check if the given Cradle is a stack cradle.
-- This might be used to determine the GHC version to use on the project.
-- If it is a stack-Cradle, we have to use `stack path --compile-exe`
-- otherwise we may ask `ghc` directly what version it is.
isStackCradle :: Cradle -> Bool
isStackCradle = (`elem` ["stack", "Cabal-Helper-Stack"]) . BIOS.actionName . BIOS.cradleOptsProg

-- | Finds a Cabal v2-project, Cabal v1-project or a Stack project
-- relative to the given FilePath.
-- Cabal v2-project and Stack have priority over Cabal v1-project.
-- This entails that if a Cabal v1-project can be identified, it is
-- first checked whether there are Stack projects or Cabal v2-projects
-- before it is concluded that this is the project root.
-- Cabal v2-projects and Stack projects are equally important.
-- Due to the lack of user-input we have to guess which project it
-- should rather be.
-- This guessing has no guarantees and may change any-time.
findCabalHelperEntryPoint :: FilePath -> IO (Maybe (Ex ProjLoc))
findCabalHelperEntryPoint fp = do
  projs <- concat <$> mapM findProjects subdirs
  case filter (\p -> isCabalNewProject p || isStackProject p) projs of
    (x:_) -> return $ Just x
    []    -> case filter isCabalOldProject projs of
      (x:_) -> return $ Just x
      []    -> return Nothing
  where
    -- | Subdirectories of a given FilePath.
    -- Directory closest to the FilePath `fp` is the head,
    -- followed by one directory taken away.
    subdirs :: [FilePath]
    subdirs = reverse . map joinPath . tail . inits $ splitDirectories (takeDirectory fp)

    isStackProject (Ex ProjLocStackYaml {}) = True
    isStackProject _ = False

    isCabalNewProject (Ex ProjLocV2Dir {}) = True
    isCabalNewProject (Ex ProjLocV2File {}) = True
    isCabalNewProject _ = False

    isCabalOldProject (Ex ProjLocV1Dir {}) = True
    isCabalOldProject (Ex ProjLocV1CabalFile {}) = True
    isCabalOldProject _ = False

-- | Given a FilePath, find the Cradle the FilePath belongs to.
--
-- TODO: document how and why this works.
cabalHelperCradle :: FilePath -> IO Cradle
cabalHelperCradle file = do
  projM <- findCabalHelperEntryPoint file
  case projM of
    Nothing        -> do
      errorm $ "Could not find a Project for file: " ++ file
      error $ "Could not find a Project for file: " ++ file
    Just (Ex proj) -> do
      -- Find the root of the project based on project type.
      let root = projectRootDir proj
      -- Create a suffix for the cradle name.
      -- Purpose is mainly for easier debugging.
      let actionNameSuffix = projectSuffix proj

      logm $ "Cabal-Helper dirs: " ++ show [root, file]

      let dist_dir = getDefaultDistDir proj
      env <- mkQueryEnv proj dist_dir
      packages <- runQuery projectPackages env
      -- Find the package the given file may belong to.
      -- If it does not belong to any package, create a none-cradle.
      -- We might want to find a cradle without actually loading anything.
      -- Useful if we only want to determine a ghc version to use.
      case packages `findPackageFor` file of
        Nothing          -> do
          debugm $ "Could not find a package for the file: " ++ file
          debugm
            "This is perfectly fine if we only want to determine the GHC version."
          return
            Cradle { cradleRootDir = root
                   , cradleOptsProg =
                       CradleAction { actionName = "Cabal-Helper-"
                                        ++ actionNameSuffix
                                        ++ "-None"
                                    , runCradle = \_ -> return CradleNone
                                    }
                   }
        Just realPackage -> do
          debugm $ "Cabal-Helper cradle package: " ++ show realPackage
          -- Field `pSourceDir` often has the form `<cwd>/./plugin`
          -- but we only want `<cwd>/plugin`
          let normalisedPackageLocation = normalise $ pSourceDir realPackage
          debugm $ "Cabal-Helper normalisedPackageLocation: " ++ normalisedPackageLocation
          return
            Cradle { cradleRootDir = normalisedPackageLocation
                   , cradleOptsProg =
                       CradleAction { actionName =
                                        "Cabal-Helper-" ++ actionNameSuffix
                                    , runCradle = cabalHelperAction
                                        env
                                        realPackage
                                        normalisedPackageLocation
                                    }
                   }
  where
    -- | Cradle Action to query for the ComponentOptions that are needed
    -- to load the given FilePath.
    -- This Function is not supposed to throw any exceptions and use
    -- 'CradleLoadResult' to indicate errors.
    cabalHelperAction :: QueryEnv v -- ^ Query Env created by 'mkQueryEnv'
                                    -- with the appropriate 'distdir'
                      -> Package v -- ^ Package this Cradle is part for.
                      -> FilePath -- ^ Absolute directory of the package.
                      -> FilePath -- ^ FilePath to load.
                      -> IO (CradleLoadResult ComponentOptions)
    cabalHelperAction env package root fp = do
      let units = pUnits package
      -- Get all unit infos the given FilePath may belong to
      -- TODO: lazily initialise units as needed
      unitInfos_ <- mapM (\unit -> runQuery (unitInfo unit) env) units
      let fpRelativeDir = takeDirectory $ makeRelative root fp
      debugm $ "Module FilePath relative to the package root: " ++ fpRelativeDir
      case getComponent unitInfos_ fpRelativeDir of
        Just comp -> do
          let fs = getFlags comp
          let targets = getTargets comp fpRelativeDir
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

-- TODO: This can be a complete match, it actually should be
getComponent :: NonEmpty UnitInfo -> FilePath -> Maybe ChComponentInfo
getComponent ui dir = listToMaybe
  $ map snd
  $ filter (hasParent dir . fst)
  $ sortOn (Down . length . fst)
  $ concatMap (\ci -> map (, ci) (ciSourceDirs ci))
  $ concat
  $ M.elems . uiComponents <$> ui

getFlags :: ChComponentInfo -> [String]
getFlags = ciGhcOptions

-- | Get all Targets of a Component, since we want to load all components.
-- FilePath is needed for the special case that the Component is an Exe.
-- The Exe contains a Path to the Main which is relative to some entry
-- in 'ciSourceDirs'.
-- We monkey-patch this by supplying the FilePath we want to load,
-- which is part of this component, and select the 'ciSourceDir' we actually want.
-- See the Documentation of 'ciCourceDir' to why this contains multiple entries.
getTargets :: ChComponentInfo -> FilePath -> [String]
getTargets comp fp = case ciEntrypoints comp of
  ChSetupEntrypoint {} -> []
  ChLibEntrypoint { chExposedModules, chOtherModules }
    -> map unChModuleName (chExposedModules ++ chOtherModules)
  ChExeEntrypoint { chMainIs, chOtherModules }
    -> [sourceDir </> chMainIs | Just sourceDir <- [sourceDirs]]
    ++ map unChModuleName chOtherModules
    where
      sourceDirs = find (`isFilePathPrefixOf` fp) (ciSourceDirs comp)

hasParent :: FilePath -> FilePath -> Bool
hasParent child parent =
  any (equalFilePath parent) (map joinPath $ inits $ splitPath child)


-- | For all packages in a project, find the project the given FilePath
-- belongs to most likely.
findPackageFor :: NonEmpty (Package pt) -> FilePath -> Maybe (Package pt)
findPackageFor packages fp = packages
  & NonEmpty.toList
  & sortOn (Down . pSourceDir)
  & filter (\p -> pSourceDir p `isFilePathPrefixOf` fp)
  & listToMaybe

-- | Helper function to make sure that both FilePaths are normalised.
--
isFilePathPrefixOf :: FilePath -> FilePath -> Bool
isFilePathPrefixOf dir fp = normalise dir `isPrefixOf` normalise fp

projectRootDir :: ProjLoc qt -> FilePath
projectRootDir ProjLocV1CabalFile { plProjectDirV1 } = plProjectDirV1
projectRootDir ProjLocV1Dir { plProjectDirV1 } = plProjectDirV1
projectRootDir ProjLocV2File { plProjectDirV2 } = plProjectDirV2
projectRootDir ProjLocV2Dir { plProjectDirV2 } = plProjectDirV2
projectRootDir ProjLocStackYaml { plStackYaml } = takeDirectory plStackYaml

projectSuffix :: ProjLoc qt -> FilePath
projectSuffix ProjLocV1CabalFile { } = "Cabal-V1"
projectSuffix ProjLocV1Dir { } =  "Cabal-V1-Dir"
projectSuffix ProjLocV2File { } = "Cabal-V2"
projectSuffix ProjLocV2Dir { } = "Cabal-V2-Dir"
projectSuffix ProjLocStackYaml { } = "Stack"

-- | The hie-bios stack cradle doesn't return the target as well, so add the
-- FilePath onto the end of the options to make sure at least one target
-- is returned.
fixCradle :: BIOS.Cradle -> BIOS.Cradle
fixCradle cradle =
  -- Normally this would also succeed for the 'Cabal-Helper-Stack' cradle.
  -- Make sure that the cradle is definitely the one created by "HIE.Bios.Cradle.loadCradle"
  if isStackCradle cradle
    -- We need a lens
    then cradle { BIOS.cradleOptsProg = (BIOS.cradleOptsProg cradle)
                { BIOS.runCradle = \fp' ->  fmap (addOption fp') <$> BIOS.runCradle (BIOS.cradleOptsProg cradle) fp' } }
    else cradle
  where
    addOption fp (BIOS.ComponentOptions os ds) = BIOS.ComponentOptions (os ++ [fp]) ds
