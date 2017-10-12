{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Haskell.Ide.BuildPlugin where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Types                       as J
import           Data.Monoid
import qualified Control.Exception as Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           GHC.Generics                                 (Generic)
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Directory (makeAbsolute, getCurrentDirectory, getDirectoryContents, doesFileExist)
import System.FilePath ((</>), normalise, takeExtension, takeFileName, makeRelative)
import System.Process (readProcess)
import System.IO (openFile, hClose, IOMode(..))

import Distribution.Helper as CH

import Distribution.Simple.Setup (defaultDistPref)
import Distribution.Simple.Configure (localBuildInfoFile)
import Distribution.Package (pkgName, unPackageName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity as Verb

import Data.Yaml

-- ---------------------------------------------------------------------
{-
buildModeArg = SParamDesc (Proxy :: Proxy "mode") (Proxy :: Proxy "Operation mode: \"stack\" or \"cabal\"") SPtText SRequired
distDirArg = SParamDesc (Proxy :: Proxy "distDir") (Proxy :: Proxy "Directory to search for setup-config file") SPtFile SOptional
toolArgs = SParamDesc (Proxy :: Proxy "cabalExe") (Proxy :: Proxy "Cabal executable") SPtText SOptional
        :& SParamDesc (Proxy :: Proxy "stackExe") (Proxy :: Proxy "Stack executable") SPtText SOptional
        :& RNil

pluginCommonArgs = buildModeArg :& distDirArg :& toolArgs


buildPluginDescriptor :: TaggedPluginDescriptor _
buildPluginDescriptor = PluginDescriptor
  {
    pdUIShortName = "Build plugin"
  , pdUIOverview = "A HIE plugin for building cabal/stack packages"
  , pdCommands =
         buildCommand prepareHelper (Proxy :: Proxy "prepare")
            "Prepares helper executable. The project must be configured first"
            [] (SCtxNone :& RNil)
            (   pluginCommonArgs
            <+> RNil) SaveNone
--       :& buildCommand isHelperPrepared (Proxy :: Proxy "isPrepared")
--             "Checks whether cabal-helper is prepared to work with this project. The project must be configured first"
--             [] (SCtxNone :& RNil)
--             (   pluginCommonArgs
--             <+> RNil) SaveNone
      :& buildCommand isConfigured (Proxy :: Proxy "isConfigured")
            "Checks if project is configured"
            [] (SCtxNone :& RNil)
            (  buildModeArg
            :& distDirArg
            :& RNil) SaveNone
      :& buildCommand configure (Proxy :: Proxy "configure")
            "Configures the project. For stack project with multiple local packages - build it"
            [] (SCtxNone :& RNil)
            (   pluginCommonArgs
            <+> RNil) SaveNone
      :& buildCommand listTargets (Proxy :: Proxy "listTargets")
            "Given a directory with stack/cabal project lists all its targets"
            [] (SCtxNone :& RNil)
            (   pluginCommonArgs
            <+> RNil) SaveNone
      :& buildCommand listFlags (Proxy :: Proxy "listFlags")
            "Lists all flags that can be set when configuring a package"
            [] (SCtxNone :& RNil)
            (  buildModeArg
            :& RNil) SaveNone
      :& buildCommand buildDirectory (Proxy :: Proxy "buildDirectory")
            "Builds all targets that correspond to the specified directory"
            [] (SCtxNone :& RNil)
            (  pluginCommonArgs
            <+> (SParamDesc (Proxy :: Proxy "directory") (Proxy :: Proxy "Directory to build targets from") SPtFile SOptional :& RNil)
            <+> RNil) SaveNone
      :& buildCommand buildTarget (Proxy :: Proxy "buildTarget")
            "Builds specified cabal or stack component"
            [] (SCtxNone :& RNil)
            (  pluginCommonArgs
            <+> (SParamDesc (Proxy :: Proxy "target") (Proxy :: Proxy "Component to build") SPtText SOptional :& RNil)
            <+> (SParamDesc (Proxy :: Proxy "package") (Proxy :: Proxy "Package to search the component in. Only applicable for Stack mode") SPtText SOptional :& RNil)
            <+> (SParamDesc (Proxy :: Proxy "type") (Proxy :: Proxy "Type of the component. Only applicable for Stack mode") SPtText SOptional :& RNil)
            <+> RNil) SaveNone
      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }
-}

buildPluginDescriptor :: PluginDescriptor
buildPluginDescriptor = PluginDescriptor
  {
    pluginName = "Build plugin"
  , pluginDesc = "A HIE plugin for building cabal/stack packages"
  , pluginCommands =
      [ PluginCommand "prepare"
                      "Prepares helper executable. The project must be configured first"
                      prepareHelper
      -- , PluginCommand "isPrepared"
      --                    ("Checks whether cabal-helper is prepared to work with this project. "
      --                  <> "The project must be configured first")
      --                  isHelperPrepared
      , PluginCommand "isConfigured"
                       "Checks if project is configured"
                       isConfigured
      , PluginCommand "configure"
                         ("Configures the project. "
                       <> "For stack project with multiple local packages - build it")
                       configure
      , PluginCommand "listTargets"
                      "Given a directory with stack/cabal project lists all its targets"
                      listTargets
      , PluginCommand "listFlags"
                      "Lists all flags that can be set when configuring a package"
                      listFlags
      , PluginCommand "buildDirectory"
                      "Builds all targets that correspond to the specified directory"
                      buildDirectory
      , PluginCommand "buildTarget"
                      "Builds specified cabal or stack component"
                      buildTarget
      ]
  }

data OperationMode = StackMode | CabalMode

readMode :: T.Text -> Maybe OperationMode
readMode "stack" = Just StackMode
readMode "cabal" = Just CabalMode
readMode _ = Nothing

-- | Used internally by commands, all fields always populated, possibly with
-- default values
data CommonArgs = CommonArgs {
         caMode :: OperationMode
        ,caDistDir :: String
        ,caCabal :: String
        ,caStack :: String
    }

-- | Used to interface with the transport, where the mode is required but rest
-- are optional
data CommonParams = CommonParams {
         cpMode    :: T.Text
        ,cpDistDir :: Maybe String
        ,cpCabal   :: Maybe String
        ,cpStack   :: Maybe String
        ,cpFile    :: Uri
    } deriving Generic

instance FromJSON CommonParams where
  parseJSON = J.genericParseJSON $ customOptions 2
instance ToJSON CommonParams where
  toJSON = J.genericToJSON $ customOptions 2

incorrectParameter :: String -> [String] -> a -> b
incorrectParameter = undefined

withCommonArgs :: MonadIO m => CommonParams -> ReaderT CommonArgs m a -> m a
withCommonArgs (CommonParams mode0 mDistDir mCabalExe mStackExe _fileUri) a = do
      case readMode mode0 of
        Nothing -> return $ incorrectParameter "mode" ["stack","cabal"] mode0
        Just mode -> do
          let cabalExe = maybe "cabal" id mCabalExe
              stackExe = maybe "stack" id mStackExe
          distDir <- maybe (liftIO $ getDistDir mode stackExe) return $
                mDistDir -- >>= uriToFilePath -- fileUri
          runReaderT a $ CommonArgs {
              caMode = mode,
              caDistDir = distDir,
              caCabal = cabalExe,
              caStack = stackExe
            }
{-
withCommonArgs req a = do
  case getParams (IdText "mode" :& RNil) req of
    Left err -> return err
    Right (ParamText mode0 :& RNil) -> do
      case readMode mode0 of
        Nothing -> return $ incorrectParameter "mode" ["stack","cabal"] mode0
        Just mode -> do
          let cabalExe = maybe "cabal" id $
                Map.lookup "cabalExe" (ideParams req) >>= (\(ParamTextP v) -> return $ T.unpack v)
              stackExe = maybe "stack" id $
                Map.lookup "stackExe" (ideParams req) >>= (\(ParamTextP v) -> return $ T.unpack v)
          distDir <- maybe (liftIO $ getDistDir mode stackExe) return $
                Map.lookup "distDir" (ideParams req) >>=
                         uriToFilePath . (\(ParamFileP v) -> v)
          runReaderT a $ CommonArgs {
              caMode = mode,
              caDistDir = distDir,
              caCabal = cabalExe,
              caStack = stackExe
            }
-}

-----------------------------------------------

-- isHelperPrepared :: CommandFunc Bool
-- isHelperPrepared = CmdSync $ \ctx req -> withCommonArgs ctx req $ do
--   distDir <- asks caDistDir
--   ret <- liftIO $ isPrepared (defaultQueryEnv "." distDir)
--   return $ IdeResponseOk ret

-----------------------------------------------

prepareHelper :: CommandFunc CommonParams ()
prepareHelper = CmdSync $ \req -> withCommonArgs req $ do
  ca <- ask
  liftIO $ case caMode ca of
      StackMode -> do
        slp <- getStackLocalPackages "stack.yaml"
        mapM_ (prepareHelper' (caDistDir ca) (caCabal ca))  slp
      CabalMode -> prepareHelper' (caDistDir ca) (caCabal ca) "."
  return $ IdeResponseOk ()

prepareHelper' :: MonadIO m => FilePath -> FilePath -> FilePath -> m ()
prepareHelper' distDir cabalExe dir =
  prepare $ (mkQueryEnv dir distDir) {qePrograms = defaultPrograms {cabalProgram = cabalExe}}

-----------------------------------------------

isConfigured :: CommandFunc CommonParams Bool
isConfigured = CmdSync $ \req -> withCommonArgs req $ do
  distDir <- asks caDistDir
  ret <- liftIO $ doesFileExist $ localBuildInfoFile distDir
  return $ IdeResponseOk ret

-----------------------------------------------

configure :: CommandFunc CommonParams ()
configure = CmdSync $ \req -> withCommonArgs req $ do
  ca <- ask
  _ <- liftIO $ case caMode ca of
      StackMode -> configureStack (caStack ca)
      CabalMode -> configureCabal (caCabal ca)
  return $ IdeResponseOk ()

configureStack :: FilePath -> IO String
configureStack stackExe = do
  slp <- getStackLocalPackages "stack.yaml"
  -- stack can configure only single local package
  case slp of
    [_singlePackage] -> readProcess stackExe ["build", "--only-configure"] ""
    _manyPackages -> readProcess stackExe ["build"] ""

configureCabal :: FilePath -> IO String
configureCabal cabalExe = readProcess cabalExe ["configure"] ""

-----------------------------------------------

data ListFlagsParams = LF { lfMode :: T.Text } deriving Generic

instance FromJSON ListFlagsParams where
  parseJSON = J.genericParseJSON $ customOptions 2
instance ToJSON ListFlagsParams where
  toJSON = J.genericToJSON $ customOptions 2

listFlags :: CommandFunc ListFlagsParams Object
listFlags = CmdSync $ \(LF mode) -> do
      cwd <- liftIO $ getCurrentDirectory
      flags0 <- liftIO $ case mode of
            "stack" -> listFlagsStack cwd
            "cabal" -> fmap (:[]) (listFlagsCabal cwd)
            _oops -> return []
      let flags' = flip map flags0 $ \(n,f) ->
                    object ["packageName" .= n, "flags" .= map flagToJSON f]
          (Object ret) = object ["res" .= toJSON flags']
      return $ IdeResponseOk ret

listFlagsStack :: FilePath -> IO [(String,[Flag])]
listFlagsStack d = do
    stackPackageDirs <- getStackLocalPackages (d </> "stack.yaml")
    mapM (listFlagsCabal . (d </>)) stackPackageDirs

listFlagsCabal :: FilePath -> IO (String,[Flag])
listFlagsCabal d = do
    [cabalFile] <- filter isCabalFile <$> getDirectoryContents d
    gpd <- readPackageDescription Verb.silent (d </> cabalFile)
    let name = unPackageName $ pkgName $ package $ packageDescription gpd
        flags' = genPackageFlags gpd
    return (name, flags')

flagToJSON :: Flag -> Value
flagToJSON f = object
        -- Cabal 2.0 changelog
	-- * Backwards incompatible change to 'FlagName' (#4062):
	--   'FlagName' is now opaque; conversion to/from 'String' now works
	--   via 'unFlagName' and 'mkFlagName' functions.

                 [ "name"        .= (unFlagName $ flagName f)
                 , "description" .= flagDescription f
                 , "default"     .= flagDefault f]

#if MIN_VERSION_Cabal(2,0,0)
#else
unFlagName :: FlagName -> String
unFlagName (FlagName s) = s
#endif

-----------------------------------------------

data BuildParams = BP {
         -- common params. horrible
         bpMode      :: T.Text
        ,bpDistDir   :: Maybe String
        ,bpCabal     :: Maybe String
        ,bpStack     :: Maybe String
        ,bpFile      :: Uri
        -- specific params
        ,bpDirectory :: Maybe Uri
    } deriving Generic

instance FromJSON BuildParams where
  parseJSON = J.genericParseJSON $ customOptions 2
instance ToJSON BuildParams where
  toJSON = J.genericToJSON $ customOptions 2

buildDirectory :: CommandFunc BuildParams ()
buildDirectory = CmdSync $ \(BP m dd c s f mbDir) -> withCommonArgs (CommonParams m dd c s f) $ do
  ca <- ask
  liftIO $ case caMode ca of
    CabalMode -> do
      -- for cabal specifying directory have no sense
      _ <- readProcess (caCabal ca) ["build"] ""
      return $ IdeResponseOk ()
    StackMode -> do
      case mbDir of
        Nothing -> do
          _ <- readProcess (caStack ca) ["build"] ""
          return $ IdeResponseOk ()
        Just dir0 -> pluginGetFile "buildDirectory" dir0 $ \dir -> do
          cwd <- getCurrentDirectory
          let relDir = makeRelative cwd $ normalise dir
          _ <- readProcess (caStack ca) ["build", relDir] ""
          return $ IdeResponseOk ()

-----------------------------------------------

data BuildTargetParams = BT {
         -- common params. horrible
         btMode      :: T.Text
        ,btDistDir   :: Maybe String
        ,btCabal     :: Maybe String
        ,btStack     :: Maybe String
        ,btFile      :: Uri
        -- specific params
        ,btTarget  :: Maybe T.Text
        ,btPackage :: Maybe T.Text
        ,btType    :: T.Text
    } deriving Generic

instance FromJSON BuildTargetParams where
  parseJSON = J.genericParseJSON $ customOptions 2
instance ToJSON BuildTargetParams where
  toJSON = J.genericToJSON $ customOptions 2

buildTarget :: CommandFunc BuildTargetParams ()
buildTarget = CmdSync $ \(BT m dd c s f component package' compType) -> withCommonArgs (CommonParams m dd c s f) $ do
  ca <- ask
  liftIO $ case caMode ca of
    CabalMode -> do
      _ <- readProcess (caCabal ca) ["build", T.unpack $ maybe "" id component] ""
      return $ IdeResponseOk ()
    StackMode -> do
      case (package', component) of
        (Just p, Nothing) -> do
          _ <- readProcess (caStack ca) ["build", T.unpack $ p `T.append` compType] ""
          return $ IdeResponseOk ()
        (Just p, Just c') -> do
          _ <- readProcess (caStack ca) ["build", T.unpack $ p `T.append` compType `T.append` (':' `T.cons` c')] ""
          return $ IdeResponseOk ()
        (Nothing, Just c') -> do
          _ <- readProcess (caStack ca) ["build", T.unpack $ ':' `T.cons` c'] ""
          return $ IdeResponseOk ()
        _ -> do
          _ <- readProcess (caStack ca) ["build"] ""
          return $ IdeResponseOk ()

-----------------------------------------------

data Package = Package {
    tPackageName :: String
   ,tDirectory :: String
   ,tTargets :: [ChComponentName]
  }

listTargets :: CommandFunc CommonParams [Value]
listTargets = CmdSync $ \req -> withCommonArgs req $ do
  ca <- ask
  targets <- liftIO $ case caMode ca of
      CabalMode -> (:[]) <$> listCabalTargets (caDistDir ca) "."
      StackMode -> listStackTargets (caDistDir ca)
  let ret = flip map targets $ \t -> object
        ["name" .= tPackageName t,
         "directory" .= tDirectory t,
         "targets" .= map compToJSON (tTargets t)]
  return $ IdeResponseOk ret

listStackTargets :: FilePath -> IO [Package]
listStackTargets distDir = do
  stackPackageDirs <- getStackLocalPackages "stack.yaml"
  mapM (listCabalTargets distDir) stackPackageDirs

listCabalTargets :: MonadIO m => FilePath -> FilePath -> m Package
listCabalTargets distDir dir = do
  runQuery (mkQueryEnv dir distDir) $ do
    pkgName' <- fst <$> packageId
    cc <- components $ (,) CH.<$> entrypoints
    let comps = map (fixupLibraryEntrypoint pkgName') $ map snd cc
    absDir <- liftIO $ makeAbsolute dir
    return $ Package pkgName' absDir comps
  where
-- # if MIN_VERSION_Cabal(2,0,0)
#if MIN_VERSION_Cabal(1,24,0)
    fixupLibraryEntrypoint n ChLibName = ChLibName
#else
    fixupLibraryEntrypoint n (ChLibName "") = (ChLibName n)
#endif
    fixupLibraryEntrypoint _ e = e

-- Example of new way to use cabal helper 'entrypoints' is a ComponentQuery,
-- components applies it to all components in the project, the semigroupoids
-- apply batches the result per component, and returns the component as the last
-- item.
getComponents :: QueryEnv -> IO [(ChEntrypoint,ChComponentName)]
getComponents env = runQuery env $ components $ (,) CH.<$> entrypoints

-----------------------------------------------

data StackYaml = StackYaml [StackPackage]
data StackPackage = LocalOrHTTPPackage { stackPackageName :: String }
                  | Repository

instance FromJSON StackYaml where
  parseJSON (Object o) = StackYaml <$>
    o .: "packages"
  parseJSON _ = mempty

instance FromJSON StackPackage where
  parseJSON (Object _) = pure Repository
  parseJSON (String s) = pure $ LocalOrHTTPPackage (T.unpack s)
  parseJSON _ = mempty

isLocal :: StackPackage -> Bool
isLocal (LocalOrHTTPPackage _) = True
isLocal _ = False

getStackLocalPackages :: FilePath -> IO [String]
getStackLocalPackages stackYamlFile = withBinaryFileContents stackYamlFile $ \contents -> do
  let (Just (StackYaml stackYaml)) = decode contents
      stackLocalPackages = map stackPackageName $ filter isLocal stackYaml
  return stackLocalPackages

compToJSON :: ChComponentName -> Value
compToJSON ChSetupHsName = object ["type" .= ("setupHs" :: T.Text)]
-- # if MIN_VERSION_Cabal(2,0,0)
#if MIN_VERSION_Cabal(1,24,0)
compToJSON ChLibName = object ["type" .= ("library" :: T.Text)]
#else
compToJSON (ChLibName n) = object ["type" .= ("library" :: T.Text), "name" .= n]
#endif
compToJSON (ChExeName n) = object ["type" .= ("executable" :: T.Text), "name" .= n]
compToJSON (ChTestName n) = object ["type" .= ("test" :: T.Text), "name" .= n]
compToJSON (ChBenchName n) = object ["type" .= ("benchmark" :: T.Text), "name" .= n]

-----------------------------------------------

getDistDir :: OperationMode -> FilePath -> IO FilePath
getDistDir CabalMode _ = do
    cwd <- getCurrentDirectory
    return $ cwd </> defaultDistPref
getDistDir StackMode stackExe = do
    cwd <- getCurrentDirectory
    dist <- init <$> readProcess stackExe ["path", "--dist-dir"] ""
    return $ cwd </> dist

isCabalFile :: FilePath -> Bool
isCabalFile f = takeExtension' f == ".cabal"

takeExtension' :: FilePath -> String
takeExtension' p =
    if takeFileName p == takeExtension p
      then "" -- just ".cabal" is not a valid cabal file
      else takeExtension p

withBinaryFileContents :: FilePath -> (B.ByteString -> IO c) -> IO c
withBinaryFileContents name act =
  Exception.bracket (openFile name ReadMode) hClose
                    (\hnd -> B.hGetContents hnd >>= act)

customOptions :: Int -> J.Options
customOptions n = J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop n}
