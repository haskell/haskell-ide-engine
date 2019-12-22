{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Haskell.Ide.Engine.Plugin.Package where

import           Haskell.Ide.Engine.MonadTypes
import qualified Haskell.Ide.Engine.Support.Hoogle as Hoogle
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Support.HieExtras as Hie
import           GHC.Generics
import           GHC.Exts
import           Control.Lens
import           Data.Aeson
import           Data.Bitraversable
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.List
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Maybe
import           Data.Monoid ((<>))
#if MIN_VERSION_Cabal(2,2,0)
import           Distribution.PackageDescription.Parsec
import           Distribution.Types.VersionRange
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L
#else
import           Distribution.PackageDescription.Parse
import           Distribution.Version
import           Haskell.Ide.Engine.Compat (isExtensionOf)
import qualified Haskell.Ide.Engine.Plugin.Package.Compat as L
#endif
import           Distribution.Types.Dependency
import           Distribution.Types.PackageName
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J
import           Distribution.Verbosity
import           System.FilePath
#if MIN_VERSION_filepath(1,4,2)
#else
#endif
import           Control.Monad.IO.Class
import           System.Directory
import           Distribution.Types.GenericPackageDescription
import           Distribution.Types.CondTree
import qualified Distribution.PackageDescription.PrettyPrint as PP
import qualified Data.Yaml as Y

packageDescriptor :: T.Text -> PluginDescriptor
packageDescriptor plId = PluginDescriptor
  { pluginId       = plId
  , pluginCommands = [PluginCommand "add" "Add a packge" addCmd]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

data AddParams = AddParams
  { rootDirParam   :: FilePath   -- ^ The root directory.
  , fileParam      :: ModulePath -- ^ A path to a module inside the
                                 -- library/executable/test-suite you want to
                                 -- add the package to. May be a relative or
                                 -- absolute path, thus, must be normalised.
  , packageParam   :: Package    -- ^ The name of the package to add.
  }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | FilePath to a cabal package description file.
type CabalFilePath = FilePath
-- | FilePath to a package.yaml package description file.
type HpackFilePath = FilePath
-- | FilePath to a module within the project.
-- May be used to establish what component the dependency shall be added to.
type ModulePath = FilePath
-- | Name of the Package to add.
type Package = T.Text

-- | Add a package to the project's dependencies.
-- May fail if no project dependency specification can be found.
-- Supported are `*.cabal` and `package.yaml` specifications.
-- Moreover, may fail with an IOException in case of a filesystem problem.
addCmd :: AddParams -> IdeGhcM (IdeResult J.WorkspaceEdit)
addCmd (AddParams rootDir modulePath pkg) = do
  packageType <- liftIO $ findPackageType rootDir
  fileMap <- reverseFileMap

  case packageType of
    CabalPackage relFp -> do
      absFp <- liftIO $ canonicalizePath relFp
      let relModulePath = makeRelative (takeDirectory absFp) modulePath

      liftToGhc $ editCabalPackage absFp relModulePath pkg fileMap
    HpackPackage relFp -> do
      absFp <- liftIO $ canonicalizePath relFp
      let relModulePath = makeRelative (takeDirectory absFp) modulePath
      liftToGhc $ editHpackPackage absFp relModulePath pkg
    NoPackage -> return $ IdeResultFail (IdeError PluginError "No package.yaml or .cabal found" Null)

data PackageType = CabalPackage FilePath -- ^ Location of Cabal File. May be relative.
                 | HpackPackage FilePath -- ^ Location of `package.yaml`. May be relative.
                 | NoPackage -- ^ No package format has been found.
                 deriving (Show, Eq)

-- | Find the package type the project with the given root uses.
-- Might have weird results if there is more than one cabal package specification
-- in the root directory.
-- The `package.yaml` is preferred in case both files are present.
-- May fail with various IOException's, for example if the given
-- directory does not exist, a Hardware Failure happens or
-- the permissions deny it.
-- However, normally this command should succeeed without any exceptions.
findPackageType :: FilePath -> IO PackageType
findPackageType rootDir = do
  files <- getDirectoryContents rootDir
  -- Search for all files that have '.cabal' as a file ending,
  -- since that is the format of the cabal file. May be more to one or zero.
  let mCabal = listToMaybe $ filter (isExtensionOf "cabal") files

  mHpack <- findFile [rootDir] "package.yaml"

  return $ fromMaybe NoPackage $ asum [HpackPackage <$> mHpack, CabalPackage <$> mCabal]

-- | Edit a hpack package to add the given package to the package.yaml.
-- If package.yaml is not in an expected format, will fail fatally.
--
-- Currently does not preserve format.
-- Keep an eye out on this other GSOC project!
-- https://github.com/wisn/format-preserving-yaml
editHpackPackage :: HpackFilePath -- ^ Path to the package.yaml file
                                  -- containing the package description.
                 -> ModulePath -- ^ Path to the module where the command has
                               -- been issued in.
                               -- Used to find out what component the
                               -- dependency shall be added to.
                 -> Package -- ^ Name of the package to add as a dependency.
                 -> IdeM (IdeResult WorkspaceEdit)
editHpackPackage fp modulePath pkgName = do
  contents <- liftIO $ B.readFile fp

  supportsDocChanges <- clientSupportsDocumentChanges

  case Y.decodeThrow contents :: Maybe Object of
    Just obj -> do
        -- Map over all major components, such as "executable", "executables",
        -- "tests" and "benchmarks". Note, that "library" is a major component,
        -- but its structure is different and can not be mapped over in the same way.
        --
        -- Only adds the package if the declared "source-dirs" field is part of the
        -- module path, or if no "source-dirs" is declared.
        let compsMapped = mapComponentTypes (ensureObject $ mapComponents (ensureObject $ mapCompDependencies addDep)) obj

        -- Is there a global "dependencies" yaml object?
        let addDepToMainDep = fromMaybe False $ do
              Array _ <- HM.lookup "dependencies" compsMapped
              return True

        -- Either add the package to only the top-level "dependencies",
        -- or to all main components of which the given module is part of.
        let newPkg
              | addDepToMainDep = mapMainDependencies addDep obj
              -- Map over the library component at last, since it has different structure.
              | otherwise = mapLibraryDependency addDep compsMapped

        let newPkgText = T.decodeUtf8 $ Y.encode newPkg

        -- Construct the WorkSpaceEdit
        let numOldLines = length $ T.lines $ T.decodeUtf8 contents
            range = J.Range (J.Position 0 0) (J.Position numOldLines 0)
            textEdit = J.TextEdit range newPkgText
            docUri = filePathToUri fp
            docId = J.VersionedTextDocumentIdentifier docUri (Just 0)
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
      let g :: T.Text -> Value -> Value
          g "dependencies" = f
          g _              = id
      in HM.mapWithKey g o

    mapLibraryDependency :: (Value -> Value) -> Object -> Object
    mapLibraryDependency f o =
      let g :: T.Text -> Value -> Value
          g "library" (Y.Object o') = Y.Object (mapCompDependencies f o')
          g _ x = x
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

-- | Takes a cabal file and a path to a module in the project and a package name to add
-- to the cabal file. Reverse file map is needed to find the correct file in the project.
-- May fail with an IOException if the Cabal file is invalid.
editCabalPackage :: CabalFilePath -- ^ Path to the cabal file to add the dependency to.
                 -> ModulePath -- ^ Path to the module that wants to add the package.
                               -- Used to find out what component the
                               -- dependency shall be added to.
                 -> Package -- ^ Name of the package added as a dependency.
                 -> (FilePath -> FilePath) -- ^ Reverse File for computing file diffs.
                 -> IdeM (IdeResult J.WorkspaceEdit)
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


    mapIfHasModule :: L.HasBuildInfo a => ModulePath -> (a -> a) -> CondTree v c a -> CondTree v c a
    mapIfHasModule modFp f = mapTreeData g
      where g x
              | null (sourceDirs x) = f x
              | hasThisModule x = f x
              | otherwise = x
            hasThisModule = any (`isPrefixOf` modFp) . sourceDirs
            sourceDirs x = x ^. L.buildInfo . L.hsSourceDirs

    -- | Add the given package name to the cabal file.
    -- Package is appended to the dependency list.
    addDep :: L.HasBuildInfo a => Package -> a -> a
    addDep dep x = L.buildInfo . L.targetBuildDepends .~ newDeps $ x
      where oldDeps = x ^. L.buildInfo . L.targetBuildDepends
            -- Add it to the bottom of the dependencies list
            -- TODO: we could sort the depencies and then insert it,
            -- or insert it in order iff the list is already sorted.
            newDeps = oldDeps ++ [Dependency (mkPackageName (T.unpack dep)) anyVersion]

-- | Provide a code action to add a package to the local package.yaml or cabal file.
-- Reads from diagnostics the unknown import module path and searches for it on Hoogle.
-- If found, offer a code action to add the package to the local package description.
codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ context = do
  mRootDir <- getRootPath
  let J.List diags = context ^. J.diagnostics
      pkgs = mapMaybe getAddablePackages diags

  -- Search for packages that define the given module.
  res <- mapM (bimapM return Hoogle.searchPackages) pkgs
  actions <- catMaybes <$> mapM (uncurry (mkAddPackageAction mRootDir)) (concatPkgs res)

  return (IdeResultOk actions)

  where
    concatPkgs = concatMap (\(d, ts) -> map (d,) ts)

    -- | Create the Add Package Action with the given diagnostics and the found package name.
    mkAddPackageAction :: Maybe FilePath -> J.Diagnostic -> Package -> IdeM (Maybe J.CodeAction)
    mkAddPackageAction mRootDir diag pkgName = case (mRootDir, J.uriToFilePath (docId ^. J.uri)) of
     (Just rootDir, Just docFp) -> do
       let title = "Add " <> pkgName <> " as a dependency"
           cmdParams = [toJSON (AddParams rootDir docFp pkgName)]
       cmd <- mkLspCommand plId "add" title (Just cmdParams)
       return $ Just (J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd))
     _ -> return Nothing

    getAddablePackages :: J.Diagnostic -> Maybe (J.Diagnostic, Package)
    getAddablePackages diag@(J.Diagnostic _ _ _ (Just "bios") msg _) = (diag,) <$> extractModuleName msg
    getAddablePackages _ = Nothing

-- | Extract a module name from an error message.
extractModuleName :: T.Text -> Maybe Package
extractModuleName msg
  | T.isPrefixOf "Could not find module " msg = Just $ Hie.extractTerm line
  | T.isPrefixOf "Could not load module " msg = Just $ Hie.extractTerm line
  | otherwise = Nothing
  where line = head $ T.lines msg

-- Example error messages
{- GHC 8.6.2 error message is

"Could not load module \8216Data.Text\8217\n" ++
"It is a member of the hidden package \8216text-1.2.3.1\8217.\n" ++
"Perhaps you need to add \8216text\8217 to the build-depends in your .cabal file.\n" ++
"Use -v to see a list of the files searched for.

-}
