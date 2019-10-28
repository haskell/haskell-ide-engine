{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Map as M
import           Data.List (inits, sortOn, isPrefixOf, find, stripPrefix)
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Ord (Down(..))
import           Data.Foldable (toList)
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
  cradleConf <- BIOS.findCradle fp
  case cradleConf of
    Just yaml -> fixCradle <$> BIOS.loadCradle yaml
    Nothing   -> cabalHelperCradle fp

-- | Check if the given Cradle is a stack cradle.
-- This might be used to determine the GHC version to use on the project.
-- If it is a stack-Cradle, we have to use `stack path --compiler-exe`
-- otherwise we may ask `ghc` directly what version it is.
isStackCradle :: Cradle -> Bool
isStackCradle = (`elem` ["stack", "Cabal-Helper-Stack"])
  . BIOS.actionName
  . BIOS.cradleOptsProg

-- | Finds a Cabal v2-project, Cabal v1-project or a Stack project
-- relative to the given FilePath.
-- Cabal v2-project and Stack have priority over Cabal v1-project.
-- This entails that if a Cabal v1-project can be identified, it is
-- first checked whether there are Stack projects or Cabal v2-projects
-- before it is concluded that this is the project root.
-- Cabal v2-projects and Stack projects are equally important.
-- Due to the lack of user-input we have to guess which project it
-- should rather be.
-- This guessing has no guarantees and may change at any time.
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
      subdirs = reverse . map joinPath . tail . inits
        $ splitDirectories (takeDirectory fp)

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
-- Finds the Cabal Package the FilePath is most likely a part of
-- and creates a cradle whose root directory is the directory
-- of the package the File belongs to.
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
          debugm
            $ "Cabal-Helper normalisedPackageLocation: "
            ++ normalisedPackageLocation
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
                        -> FilePath -- ^ Root directory of the cradle
                                    -- this action belongs to.
                        -> FilePath -- ^ FilePath to load, expected to be an absolute path.
                        -> IO (CradleLoadResult ComponentOptions)
      cabalHelperAction env package root fp = do
        -- Get all unit infos the given FilePath may belong to
        let units = pUnits package
        -- make the FilePath to load relative to the root of the cradle.
        let relativeFp = makeRelative root fp
        debugm $ "Relative Module FilePath: " ++ relativeFp
        getComponent env (toList units) relativeFp
          >>= \case
            Just comp -> do
              let fs = getFlags comp
              let targets = getTargets comp relativeFp
              let ghcOptions = fs ++ targets
              debugm $ "Flags for \"" ++ fp ++ "\": " ++ show ghcOptions
              debugm $ "Component Infos: " ++ show comp
              return
                $ CradleSuccess
                  ComponentOptions { componentOptions = ghcOptions
                                   , componentDependencies = []
                                   }
            Nothing   -> return
              $ CradleFail
              $ CradleError
                (ExitFailure 2)
                ("Could not obtain flags for " ++ fp)

-- | Get the component the given FilePath most likely belongs to.
-- Lazily ask units whether the given FilePath is part of their component.
-- If a Module belongs to multiple components, it is not specified which
-- component will be loaded.
-- The given FilePath must be relative to the Root of the project
-- the given units belong to.
getComponent
  :: QueryEnv pt -> [Unit pt] -> FilePath -> IO (Maybe ChComponentInfo)
getComponent _env [] _fp = return Nothing
getComponent env (unit:units) fp = do
  ui <- runQuery (unitInfo unit) env
  let components = M.elems (uiComponents ui)
  debugm $ "Unit Info: " ++ show ui
  case find (fp `partOfComponent`) components of
    Nothing -> getComponent env units fp
    comp {- Just component -} -> return comp

-- | Check whether the given FilePath is part of the Component.
-- A FilePath is part of the Component if and only if:
--
--   * One Component's 'ciSourceDirs' is a prefix of the FilePath
--   * The FilePath, after converted to a Module name,
--     is a in the Component's Targets, or the FilePath is
--     the executable in the component.
--
-- The latter is achieved by making the FilePath relative to the 'ciSourceDirs'
-- and then replacing Path separators with ".".
-- To check whether the given FilePath is the executable of the Component,
-- we have to check whether the FilePath, including 'ciSourceDirs',
-- is part of the targets in the Component.
partOfComponent ::
  -- | FilePath relative to the package root.
  FilePath ->
  -- | Component to check whether the given FilePath is part of it.
  ChComponentInfo ->
  Bool
partOfComponent fp comp
  | Just normFp <- normalisedFp fp (ciSourceDirs comp), normFp `inTargets` getTargets comp fp = True
  | otherwise = False
  where
    -- | Assuming a FilePath "src/Lib/Lib.hs" and a ciSourceDirs ["src"]
    -- into 'Just "Lib"'
    -- >>> normalisedFp "src/Lib/Lib.hs" ["src"]
    -- Just "Lib/Lib.hs"
    --
    -- >>> normalisedFp "src/Lib/Lib.hs" ["app"]
    -- Nothing
    normalisedFp file sourceDirs = listToMaybe
      $ mapMaybe ((`stripPrefix` file) . addTrailingPathSeparator) sourceDirs

    inTargets :: FilePath -> [String] -> Bool
    inTargets modFp targets =
      -- Change a FilePath of the Form "Haskell/IDE/Engine/Cradle.hs" -> "Haskell.IDE.Engine.Cradle"
      let modName = map
            (\c -> if isPathSeparator c
                   then '.'
                   else c)
            (dropExtension modFp)
      in any (`elem` targets) [modName, fp]

-- | Get the flags necessary to compile the given component.
getFlags :: ChComponentInfo -> [String]
getFlags = ciGhcOptions

-- | Get all Targets of a Component, since we want to load all components.
-- FilePath is needed for the special case that the Component is an Exe.
-- The Exe contains a Path to the Main which is relative to some entry
-- in 'ciSourceDirs'.
-- We monkey-patch this by supplying the FilePath we want to load,
-- which is part of this component, and select the 'ciSourceDir' we actually want.
-- See the Documentation of 'ciSourceDir' to why this contains multiple entries.
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

-- | For all packages in a project, find the project the given FilePath
-- belongs to most likely.
findPackageFor :: NonEmpty (Package pt) -> FilePath -> Maybe (Package pt)
findPackageFor packages fp = packages
  & NonEmpty.toList
  & sortOn (Down . pSourceDir)
  & filter (\p -> pSourceDir p `isFilePathPrefixOf` fp)
  & listToMaybe

-- | Helper function to make sure that both FilePaths are normalised.
-- Checks whether the first FilePath is a Prefix of the second FilePath.
-- Intended usage:
--
-- >>> isFilePathPrefixOf "./src/" "./src/File.hs"
-- True
--
-- >>> isFilePathPrefixOf "./src" "./src/File.hs"
-- True
--
-- >>> isFilePathPrefixOf "./src/././" "./src/File.hs"
-- True
--
-- >>> isFilePathPrefixOf "./src" "./src-dir/File.hs"
-- True -- This is not really intended.
isFilePathPrefixOf :: FilePath -> FilePath -> Bool
isFilePathPrefixOf dir fp = normalise dir `isPrefixOf` normalise fp

projectRootDir :: ProjLoc qt -> FilePath
projectRootDir ProjLocV1CabalFile { plProjectDirV1 } = plProjectDirV1
projectRootDir ProjLocV1Dir { plProjectDirV1 } = plProjectDirV1
projectRootDir ProjLocV2File { plProjectDirV2 } = plProjectDirV2
projectRootDir ProjLocV2Dir { plProjectDirV2 } = plProjectDirV2
projectRootDir ProjLocStackYaml { plStackYaml } = takeDirectory plStackYaml

projectSuffix :: ProjLoc qt -> FilePath
projectSuffix ProjLocV1CabalFile {} = "Cabal-V1"
projectSuffix ProjLocV1Dir {} = "Cabal-V1-Dir"
projectSuffix ProjLocV2File {} = "Cabal-V2"
projectSuffix ProjLocV2Dir {} = "Cabal-V2-Dir"
projectSuffix ProjLocStackYaml {} = "Stack"

-- | The hie-bios stack cradle doesn't return the target as well, so add the
-- FilePath onto the end of the options to make sure at least one target
-- is returned.
fixCradle :: BIOS.Cradle -> BIOS.Cradle
fixCradle cradle =
  -- Normally this would also succeed for the 'Cabal-Helper-Stack' cradle.
  -- Make sure that the cradle is definitely the one created by "HIE.Bios.Cradle.loadCradle"
  if isStackCradle cradle
  then
    -- We need a lens
    cradle { BIOS.cradleOptsProg =
               (BIOS.cradleOptsProg
                  cradle) { BIOS.runCradle = \fp' -> fmap (addOption fp')
                              <$> BIOS.runCradle
                                (BIOS.cradleOptsProg cradle)
                                fp'
                          }
           }
  else cradle
  where
    addOption fp (BIOS.ComponentOptions os ds) =
      BIOS.ComponentOptions (os ++ [fp]) ds
