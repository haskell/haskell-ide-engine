{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
module Haskell.Ide.Engine.Plugin.Package where

import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.MonadFunctions (logm)
import           Haskell.Ide.Engine.PluginUtils
import           GHC.Generics
import           Data.Aeson
import           Control.Lens
import           Data.List
import qualified Data.Text                     as T
import           Data.Maybe
import           Distribution.PackageDescription.Parsec
import           Distribution.Types.Dependency
import           Distribution.Types.PackageName
import           Distribution.Types.VersionRange
import qualified Language.Haskell.LSP.Types    as J
import           Distribution.Verbosity
import           System.FilePath
import           Control.Monad.IO.Class
import           System.Directory
import qualified GhcMod.Utils                  as GM
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import           Distribution.Types.GenericPackageDescription
import           Distribution.Types.CondTree
import qualified Distribution.PackageDescription.PrettyPrint as PP
import qualified Distribution.Types.BuildInfo.Lens as L

packageDescriptor :: PluginDescriptor
packageDescriptor = PluginDescriptor
  { pluginName     = "package"
  , pluginDesc     = "Tools for editing .cabal and package.yaml files."
  , pluginCommands = [PluginCommand "add" "Add a packge" addCmd]
  }

data AddParams = AddParams FilePath -- ^ The root directory.
                           FilePath -- ^ A path to a module inside the
                                    -- library/executable/test-suite you want to
                                    -- add the package to.
                           T.Text -- ^ The name of the package to add.
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

addCmd :: CommandFunc AddParams J.WorkspaceEdit
addCmd = CmdSync $ \(AddParams rootDir modulePath pkg) -> do
  --TODO: Package.yaml

  logm $ "Got args: " ++ show rootDir ++ show pkg
  fileMap <- GM.mkRevRedirMapFunc

  files <- liftIO $ getDirectoryContents rootDir
  let maybeCabal = listToMaybe $ filter (isExtensionOf "cabal") files
  case maybeCabal of
    Just cabal -> liftIO $ do
      absCabal <- canonicalizePath cabal
      newPackage <- editPackage absCabal modulePath (T.unpack pkg)
      edit <- makeAdditiveDiffResult absCabal (T.pack newPackage) fileMap
      return $ IdeResultOk edit
    _           -> error "No .cabal"


-- | Takes a cabal file and a path to a module in the dependency you want to edit.
editPackage :: FilePath -> FilePath ->  String -> IO String
editPackage file modulePath pkgName = do

  package <- liftIO $ readGenericPackageDescription normal file

  let newMainLib = fmap updateTree $ package ^. L.condLibrary
      swappedMainLib = L.condLibrary .~ newMainLib $ package
      newPackage = applyLens L.condSubLibraries
                    $ applyLens L.condTestSuites
                    $ applyLens L.condExecutables
                    $ applyLens L.condBenchmarks swappedMainLib

  return $ PP.showGenericPackageDescription newPackage

  where relModulePath = makeRelative (takeDirectory file) modulePath

        applyLens :: L.HasBuildInfo a => Lens' GenericPackageDescription [(b, CondTree v c a)]
                  -> GenericPackageDescription -> GenericPackageDescription
        applyLens l pkg =
          let old = pkg ^. l
              new = map (\(name, tree) -> (name, updateTree tree)) old
          in l .~ new $ pkg

        updateTree :: L.HasBuildInfo a => CondTree v c a -> CondTree v c a
        updateTree = mapIfHasModule relModulePath (addDep pkgName)


mapIfHasModule :: L.HasBuildInfo a => FilePath -> (a -> a) -> CondTree v c a -> CondTree v c a
mapIfHasModule modFp f = mapTreeData g
  where g x = if hasThisModule x
                then f x
                else x
        hasThisModule x = any (`isPrefixOf` modFp) (x ^. L.buildInfo . L.hsSourceDirs)

addDep :: L.HasBuildInfo a => String -> a -> a
addDep dep x = L.buildInfo . L.targetBuildDepends .~ newDeps $ x
  where oldDeps = x ^. L.buildInfo . L.targetBuildDepends
        -- Add it to the bottom of the dependencies list
        newDeps = oldDeps ++ [Dependency (mkPackageName dep) anyVersion]