{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
module Haskell.Ide.Engine.Plugin.Package where

import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           GHC.Generics
import           GHC.Exts
import           Data.Aeson
import           Control.Lens
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.List
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Maybe
#if MIN_VERSION_Cabal(2,2,0)
import           Distribution.PackageDescription.Parsec
import           Distribution.Types.VersionRange
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L
#else
import           Distribution.PackageDescription.Parse
import           Distribution.Version
import qualified Haskell.Ide.Engine.Compat as L
#endif
import           Distribution.Types.Dependency
import           Distribution.Types.PackageName
import qualified Language.Haskell.LSP.Types    as J
import           Distribution.Verbosity
import           System.FilePath
#if MIN_VERSION_filepath(1,4,2)
#else
import            Haskell.Ide.Engine.Compat (isExtensionOf)
#endif
import           Control.Monad.IO.Class
import           System.Directory
import qualified GhcMod.Utils                  as GM
import           Distribution.Types.GenericPackageDescription
import           Distribution.Types.CondTree
import qualified Distribution.PackageDescription.PrettyPrint as PP
import qualified Data.Yaml as Y

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

  packageType <- liftIO $ findPackageType rootDir
  fileMap <- GM.mkRevRedirMapFunc

  case packageType of
    CabalPackage relFp -> do
      absFp <- liftIO $ canonicalizePath relFp
      let relModulePath = makeRelative (takeDirectory absFp) modulePath
      
      liftToGhc $ editCabalPackage absFp relModulePath (T.unpack pkg) fileMap
    HpackPackage relFp -> do
      absFp <- liftIO $ canonicalizePath relFp
      let relModulePath = makeRelative (takeDirectory absFp) modulePath
      liftToGhc $ editHpackPackage absFp relModulePath pkg
    NoPackage -> return $ IdeResultFail (IdeError PluginError "No package.yaml or .cabal found" Null)

data PackageType = CabalPackage FilePath
                 | HpackPackage FilePath
                 | NoPackage

findPackageType :: FilePath -> IO PackageType
findPackageType rootDir = do
  files <- getDirectoryContents rootDir
  let mCabal = listToMaybe $ filter (isExtensionOf "cabal") files

  mHpack <- findFile [rootDir] "package.yaml"

  return $ fromMaybe NoPackage $ asum [HpackPackage <$> mHpack, CabalPackage <$> mCabal]

-- Currently does not preserve format.
-- Keep an eye out on this other GSOC project!
-- https://github.com/wisn/format-preserving-yaml
editHpackPackage :: FilePath -> FilePath -> T.Text -> IdeM (IdeResult WorkspaceEdit)
editHpackPackage fp modulePath pkgName = do
  contents <- liftIO $ B.readFile fp

  supportsDocChanges <- clientSupportsDocumentChanges

  case Y.decode contents :: Maybe Object of
    Just obj -> do
        let compsMapped = mapComponentTypes (ensureObject $ mapComponents (ensureObject $ mapCompDependencies addDep)) obj

        let addDepToMainLib = fromMaybe True $ do
              Object lib <- HM.lookup "library" compsMapped
              sourceDirs <- HM.lookup "source-dirs" lib
              return $ isInSourceDir sourceDirs

        let newPkg = if addDepToMainLib
            then mapMainDependencies addDep compsMapped
            else compsMapped

            newPkgText = T.decodeUtf8 $ Y.encode newPkg

        let numOldLines = length $ T.lines $ T.decodeUtf8 contents
            range = J.Range (J.Position 0 0) (J.Position numOldLines 0)
            textEdit = J.TextEdit range newPkgText
            docUri = filePathToUri fp
            docId = J.VersionedTextDocumentIdentifier docUri 0
            textDocEdit = J.TextDocumentEdit docId (J.List [textEdit])
            wsEdit =
              if supportsDocChanges
                then J.WorkspaceEdit Nothing (Just (J.List [textDocEdit]))
                else J.WorkspaceEdit (Just (HM.singleton docUri (J.List [textEdit]))) Nothing

        return $ IdeResultOk wsEdit
    Nothing -> return $ IdeResultFail (IdeError PluginError "Couldn't parse package.yaml" Null)

  where

    mapMainDependencies :: (Value -> Value) -> Object -> Object
    mapMainDependencies f o =
      let g "dependencies" = f
          g _              = id
      in HM.mapWithKey g o

    mapComponentTypes :: (Value -> Value) -> Object -> Object
    mapComponentTypes f o =
      let g "executables" = f
          g "executable"  = f
          g "benchmarks"  = f
          g "tests"       = f
          g _             = id
      in HM.mapWithKey g o

    mapComponents :: (Value -> Value) -> Object -> Object
    mapComponents = HM.map

    mapCompDependencies :: (Value -> Value) -> Object -> Object
    mapCompDependencies f o =
      let g "dependencies" = f
          g _              = id
          shouldMap = ("source-dirs" `notElem` HM.keys o) || hasModule
          hasModule = maybe False isInSourceDir $ HM.lookup "source-dirs" o
      in if shouldMap
          then HM.mapWithKey g o
          else o

    isInSourceDir :: Value -> Bool
    isInSourceDir (Array sourceDirs) = any containsPrefix sourceDirs
    isInSourceDir x = containsPrefix x

    containsPrefix (String s) = s `T.isPrefixOf` T.pack modulePath
    containsPrefix _ = False

    ensureObject :: (Object -> Object) -> (Value -> Value)
    ensureObject f v = case v of
      Object o -> Object (f o)
      _ -> error "Not an object!"

    addDep (Array deps) = Array $ fromList (String pkgName:GHC.Exts.toList deps)
    addDep _            = error "Not an array in addDep"


-- | Takes a cabal file and a path to a module in the dependency you want to edit.
editCabalPackage :: FilePath -> FilePath ->  String -> (FilePath -> FilePath) -> IdeM (IdeResult J.WorkspaceEdit)
editCabalPackage file modulePath pkgName fileMap = do

  package <- liftIO $ readGenericPackageDescription normal file

  let newMainLib = fmap updateTree $ package ^. L.condLibrary
      swappedMainLib = L.condLibrary .~ newMainLib $ package
      newPackage = applyLens L.condSubLibraries
                    $ applyLens L.condTestSuites
                    $ applyLens L.condExecutables
                    $ applyLens L.condBenchmarks swappedMainLib

  let newContents = T.pack $ PP.showGenericPackageDescription newPackage

  IdeResultOk <$> makeAdditiveDiffResult file newContents fileMap


  where

    applyLens :: L.HasBuildInfo a => Lens' GenericPackageDescription [(b, CondTree v c a)]
              -> GenericPackageDescription -> GenericPackageDescription
    applyLens l pkg =
      let old = pkg ^. l
          new = map (\(name, tree) -> (name, updateTree tree)) old
      in l .~ new $ pkg

    updateTree :: L.HasBuildInfo a => CondTree v c a -> CondTree v c a
    updateTree = mapIfHasModule modulePath (addDep pkgName)


    mapIfHasModule :: L.HasBuildInfo a => FilePath -> (a -> a) -> CondTree v c a -> CondTree v c a
    mapIfHasModule modFp f = mapTreeData g
      where g x
              | null (sourceDirs x) = f x
              | hasThisModule x = f x
              | otherwise = x
            hasThisModule = any (`isPrefixOf` modFp) . sourceDirs
            sourceDirs x = x ^. L.buildInfo . L.hsSourceDirs

    addDep :: L.HasBuildInfo a => String -> a -> a
    addDep dep x = L.buildInfo . L.targetBuildDepends .~ newDeps $ x
      where oldDeps = x ^. L.buildInfo . L.targetBuildDepends
            -- Add it to the bottom of the dependencies list
            newDeps = oldDeps ++ [Dependency (mkPackageName dep) anyVersion]