{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Haskell.Ide.BuildPlugin where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception as Exception
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Reader
import           Haskell.Ide.Engine.ExtensibleState
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import Language.Haskell.LSP.TH.DataTypesJSON (uriToFilePath)
import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Monoid
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Directory (makeAbsolute, getCurrentDirectory, getDirectoryContents, doesFileExist)
import System.FilePath ((</>), normalise, takeExtension, takeFileName)
import System.Process (readProcess)
import System.IO (openFile, hClose, IOMode(..))
import System.IO.Error

import Distribution.Helper

import Distribution.Simple.Setup (defaultDistPref)
import Distribution.Simple.Configure (localBuildInfoFile)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils (findPackageDesc, withFileContents)
import Distribution.Package (pkgName, unPackageName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity as Verb

import Data.Yaml

-- ---------------------------------------------------------------------

pluginCommonArgs = SParamDesc (Proxy :: Proxy "mode") (Proxy :: Proxy "Operation mode: \"stack\" or \"cabal\"") SPtText SRequired
            :& SParamDesc (Proxy :: Proxy "distDir") (Proxy :: Proxy "Directory to search for setup-config file") SPtFile SOptional
            :& SParamDesc (Proxy :: Proxy "cabalExe") (Proxy :: Proxy "Cabal executable") SPtText SOptional
            :& SParamDesc (Proxy :: Proxy "stackExe") (Proxy :: Proxy "Stack executable") SPtText SOptional
            :& RNil

buildPluginDescriptor :: TaggedPluginDescriptor _
buildPluginDescriptor = PluginDescriptor
  {
    pdUIShortName = "Build plugin"
  , pdUIOverview = "A HIE plugin for building cabal/stack packages"
  , pdCommands =
        buildCommand isHelperPrepared (Proxy :: Proxy "isPrepared")
            "Checks whether cabal-helper is prepared to work with this project. The project must be configured first"
            [] (SCtxNone :& RNil)
            (   pluginCommonArgs
            <+> RNil) SaveNone
      :& buildCommand prepareHelper (Proxy :: Proxy "prepare")
            "Prepares helper executable. The project must be configured first"
            [] (SCtxNone :& RNil)
            (   pluginCommonArgs
            <+> RNil) SaveNone
      :& buildCommand isConfigured (Proxy :: Proxy "isConfigured")
            "Checks if project is configured"
            [] (SCtxNone :& RNil)
            (  SParamDesc (Proxy :: Proxy "distDir") (Proxy :: Proxy "Directory to search for setup-config file") SPtFile SOptional
            :& SParamDesc (Proxy :: Proxy "mode") (Proxy :: Proxy "Operation mode: \"stack\" or \"cabal\"") SPtText SRequired
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
            (  SParamDesc (Proxy :: Proxy "mode") (Proxy :: Proxy "Project type: \"stack\" or \"cabal\"") SPtText SRequired
            :& RNil) SaveNone
      :& buildCommand addTarget (Proxy :: Proxy "addTarget") "Add a new target to the cabal file" [] (SCtxNone :& RNil)
            (  SParamDesc (Proxy :: Proxy "file") (Proxy :: Proxy "Path to the .cabal file") SPtFile SRequired
            :& SParamDesc (Proxy :: Proxy "name") (Proxy :: Proxy "Name of the new target") SPtText SRequired
            :& SParamDesc (Proxy :: Proxy "type") (Proxy :: Proxy "executable/library") SPtText SRequired
            :& RNil) SaveNone
      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

data OperationMode = StackMode | CabalMode

readMode "stack" = Just StackMode
readMode "cabal" = Just CabalMode
readMode _ = Nothing

data CommonArgs = CommonArgs {
         caMode :: OperationMode
        ,caDistDir :: String
        ,caCabal :: String
        ,caStack :: String
    }

--withCommonArgs :: [AcceptedContext] -> IdeRequest -> ReaderT CommonArgs IdeM (IdeResponse resp) -> IdeM (IdeResponse resp)
withCommonArgs ctx req a = do
  case getParams (IdText "mode" :& RNil) req of
    Left err -> return err
    Right (ParamText mode0 :& RNil) -> do
      case readMode mode0 of
        Nothing -> return $ incorrectParameter "mode" ["stack","cabal"] mode0
        Just mode -> do
          let distDir0 = maybe "" id $
                Map.lookup "distDir" (ideParams req) >>=
                         uriToFilePath . (\(ParamFileP v) -> v)
              cabalExe = maybe "cabal" id $
                Map.lookup "cabalExe" (ideParams req) >>= (\(ParamTextP v) -> return $ T.unpack v)
              stackExe = maybe "stack" id $
                Map.lookup "stackExe" (ideParams req) >>= (\(ParamTextP v) -> return $ T.unpack v)
          distDir <- liftIO $ withDistDir mode distDir0 return
          runReaderT a $ CommonArgs {
              caMode = mode,
              caDistDir = distDir,
              caCabal = cabalExe,
              caStack = stackExe
            }

-----------------------------------------------

isHelperPrepared :: CommandFunc Bool
isHelperPrepared = CmdSync $ \ctx req -> withCommonArgs ctx req $ do
  distDir <- asks caDistDir
  ret <- liftIO $ isPrepared (defaultQueryEnv "." distDir)
  return $ IdeResponseOk ret

-----------------------------------------------

prepareHelper :: CommandFunc ()
prepareHelper = CmdSync $ \ctx req -> withCommonArgs ctx req $ do
  ca <- ask
  liftIO $ case caMode ca of
      StackMode -> do
        slp <- getStackLocalPackages "stack.yaml"
        mapM_ (prepareHelper' (caDistDir ca) (caCabal ca))  slp
      CabalMode -> prepareHelper' (caDistDir ca) (caCabal ca) "."
  return $ IdeResponseOk ()

prepareHelper' distDir cabalExe dir =
  prepare' $ (defaultQueryEnv dir distDir) {qePrograms = defaultPrograms {cabalProgram = cabalExe}}

-----------------------------------------------

isConfigured :: CommandFunc Bool
isConfigured = CmdSync $ \ctx req -> withCommonArgs ctx req $ do
  distDir <- asks caDistDir
  ret <- liftIO $ doesFileExist $ localBuildInfoFile distDir
  return $ IdeResponseOk ret

-----------------------------------------------

configure :: CommandFunc ()
configure = CmdSync $ \ctx req -> withCommonArgs ctx req $ do
  ca <- ask
  liftIO $ case caMode ca of
      StackMode -> configureStack' (caStack ca)
      CabalMode -> configureCabal' (caCabal ca)
  return $ IdeResponseOk ()

configureStack' stackExe = do
  slp <- getStackLocalPackages "stack.yaml"
  -- stack can configur only single local package
  case slp of
    [singlePackage] -> readProcess stackExe ["build", "--only-configure"] ""
    manyPackages -> readProcess stackExe ["build"] ""
configureCabal' cabalExe = readProcess cabalExe ["configure"] ""

-----------------------------------------------

listFlags :: CommandFunc Object
listFlags = CmdSync $ \ctx req -> do
  case getParams (IdText "mode" :& RNil) req of
    Left err -> return err
    Right (ParamText mode :& RNil) -> do
      cwd <- liftIO $ getCurrentDirectory
      flags0 <- liftIO $ case mode of
            "stack" -> listFlagsStack cwd
            "cabal" -> fmap (:[]) (listFlagsCabal cwd)
      let flags = flip map flags0 $ \(n,f) ->
                    object ["packageName" .= n, "flags" .= map flagToJSON f]
          (Object ret) = object ["res" .= toJSON flags]
      return $ IdeResponseOk ret

listFlagsStack d = do
    stackPackageDirs <- getStackLocalPackages (d </> "stack.yaml")
    mapM (listFlagsCabal . (d </>)) stackPackageDirs

listFlagsCabal d = do
    [cabalFile] <- filter isCabalFile <$> getDirectoryContents d
    gpd <- readPackageDescription Verb.silent (d </> cabalFile)
    let name = unPackageName $ pkgName $ package $ packageDescription gpd
        flags = genPackageFlags gpd
    return (name, flags)

flagToJSON f = object ["name" .= ((\(FlagName s) -> s) $ flagName f), "description" .= flagDescription f, "default" .= flagDefault f]

-----------------------------------------------

listConfigFlags :: CommandFunc Object
listConfigFlags = CmdSync $ \ctx req -> do
  case getParams (IdFile "directory" :& IdText "type" :& RNil) req of
    Left err -> return err
    Right (ParamFile dir0 :& ParamText type_ :& RNil) ->
      pluginGetFile "listConfigFlags" dir0 $ \dir -> do
        flags0 <- liftIO $ listConfigFlags' type_ dir
        let flags = flip map flags0 $ \(n, d, fs) ->
                object ["name" .= n, "directory" .= d, "flags" .= map configFlagToJSON fs]
            (Object ret) = object ["res" .= toJSON flags]
        return $ IdeResponseOk ret

listConfigFlags' type_ dir = do
  case type_ of
    "stack" -> do
      stackPackageDirs <- getStackLocalPackages (dir </> "stack.yaml")
      concat <$> mapM (listConfigFlags' "cabal") stackPackageDirs
    "cabal" -> runQuery (defaultQueryEnv dir "") $ do
      (pkgName, _) <- packageId
      fs <- flags
      return [(pkgName, dir, fs)]

configFlagToJSON (n,v) = object ["name" .= n, "default" .= v, "value" .= v]

-----------------------------------------------

data Package = Package {
    tPackageName :: String
   ,tDirectory :: String
   ,tTargets :: [ChComponentName]
  }

listTargets :: CommandFunc [Value]
listTargets = CmdSync $ \ctx req -> withCommonArgs ctx req $ do
  ca <- ask
  targets <- liftIO $ case caMode ca of
      CabalMode -> (:[]) <$> listCabalTargets' (caDistDir ca) "."
      StackMode -> listStackTargets' (caDistDir ca)
  let ret = flip map targets $ \t -> object
        ["name" .= tPackageName t,
         "directory" .= tDirectory t,
         "targets" .= map compToJSON (tTargets t)]
  return $ IdeResponseOk ret

listStackTargets' distDir = do
  stackPackageDirs <- getStackLocalPackages "stack.yaml"
  mapM (listCabalTargets' distDir) stackPackageDirs

listCabalTargets' distDir dir = do
  runQuery (defaultQueryEnv dir distDir) $ do
    pkgName <- fst <$> packageId
    comps <- map (fixupLibraryEntrypoint pkgName) <$> map fst <$> entrypoints
    absDir <- liftIO $ makeAbsolute dir
    return $ Package pkgName absDir comps
  where
    fixupLibraryEntrypoint n (ChLibName "") = (ChLibName n)
    fixupLibraryEntrypoint _ e = e

-----------------------------------------------

data StackYaml = StackYaml [StackPackage]
data StackPackage = LocalOrHTTPPackage { stackPackageName :: String }
                  | Repository

instance FromJSON StackYaml where
  parseJSON (Object o) = StackYaml <$>
    o .: "packages"

instance FromJSON StackPackage where
  parseJSON (Object o) = pure Repository
  parseJSON (String s) = pure $ LocalOrHTTPPackage (T.unpack s)

isLocal (LocalOrHTTPPackage _) = True
isLocal _ = False

getStackLocalPackages stackYaml = withBinaryFileContents stackYaml $ \contents -> do
  let (Just (StackYaml stackYaml)) = decode contents
      stackLocalPackages = map stackPackageName $ filter isLocal stackYaml
  return stackLocalPackages

compToJSON ChSetupHsName = object ["type" .= ("setupHs" :: T.Text)]
compToJSON (ChLibName n) = object ["type" .= ("library" :: T.Text), "name" .= n]
compToJSON (ChExeName n) = object ["type" .= ("executable" :: T.Text), "name" .= n]
compToJSON (ChTestName n) = object ["type" .= ("test" :: T.Text), "name" .= n]
compToJSON (ChBenchName n) = object ["type" .= ("benchmark" :: T.Text), "name" .= n]

-----------------------------------------------

withDistDir CabalMode "" f = do
    cwd <- getCurrentDirectory
    f $ cwd </> defaultDistPref
withDistDir StackMode "" f = do
    cwd <- getCurrentDirectory
    dist <- getStackDistDir
    f $ cwd </> dist

isCabalFile :: FilePath -> Bool
isCabalFile f = takeExtension' f == ".cabal"

takeExtension' :: FilePath -> String
takeExtension' p =
    if takeFileName p == takeExtension p
      then "" -- just ".cabal" is not a valid cabal file
      else takeExtension p

withBinaryFileContents name act =
  Exception.bracket (openFile name ReadMode) hClose
                    (\hnd -> B.hGetContents hnd >>= act)

-- TODO: it would be nice to cache this somehow
getStackDistDir :: IO FilePath
getStackDistDir = init <$> readProcess "stack" ["path", "--dist-dir"] ""

addTarget = CmdSync $ \ctx req -> do
  let args = (,,) <$> Map.lookup "file" (ideParams req)
                  <*> Map.lookup "name" (ideParams req)
                  <*> Map.lookup "type" (ideParams req)
  case args of
      Nothing -> return $ missingParameter "file, name or type"
      Just (ParamFileP file0, ParamTextP name, ParamTextP _type) ->
        pluginGetFile "addTarget" file0 $ \file -> do
          res <- liftIO $ addTarget' file (T.unpack name) (T.unpack _type)
          return $ case res of
              False -> missingParameter "meh"
              True -> IdeResponseOk ()

addTarget' :: FilePath -> String -> String -> IO Bool
addTarget' file name _type = do
    parseRes <- parsePackageDescription <$> readFile file
    case parseRes of
        ParseFailed _ -> return False
        ParseOk _ genPackDesc -> do
            let newDescr = case _type of
                    "executable" -> genPackDesc {
                        condExecutables = condExecutables genPackDesc <>
                            [(name, CondNode emptyExecutable [] [])]
                        }
            --writePackageDescription file newDescr
            return True

data WorkerCmd = Cmd1 | Cmd2
              deriving Show

-- | Keep track of the communication channesl to the remote process.
data SubProcess = SubProcess
  { spChIn    :: TChan WorkerCmd
  , spChOut   :: TChan T.Text
  , spProcess :: ThreadId
  }

-- | Wrap it in a Maybe for pure initialisation
data AsyncPluginState = APS (Maybe SubProcess)

-- | Tag the state variable to enable it to be stored in the dispatcher state,
-- accessible to all plugins, provided they know the type, as it is accessed via
-- a @cast@
instance ExtensionClass AsyncPluginState where
  initialValue = APS Nothing

-- | This command manages interaction with a separate process, doing stuff.
longRunningCmdSync :: WorkerCmd -> CommandFunc T.Text
longRunningCmdSync cmd = CmdSync $ \_ctx req -> do
  SubProcess cin cout _tid <- ensureProcessRunning
  liftIO $ atomically $ writeTChan cin cmd
  res <- liftIO $ atomically $ readTChan cout
  return (IdeResponseOk $ "res=" <> res)

-- ---------------------------------------------------------------------

-- | If there is already a @SubProcess@ value in the plugin state return it,
-- else create a new set of @TChan@ and fork the worker with them, storing the
-- new @SubProcess@ value in the plugin state.
ensureProcessRunning :: IdeM SubProcess
ensureProcessRunning = do
  (APS v) <- get -- from extensible state
  sp <- case v of
    Nothing -> do
      cin  <- liftIO $ atomically newTChan
      cout <- liftIO $ atomically newTChan
      tid  <- liftIO $ forkIO (workerProc cin cout)
      let v' = SubProcess cin cout tid
      put (APS (Just v')) -- into extensible state
      return v'
    Just v' -> return v'
  return sp

-- ---------------------------------------------------------------------

-- | Long running worker process, can be doing commands in an async manner
workerProc :: TChan WorkerCmd -> TChan T.Text -> IO ()
workerProc cin cout = loop 1
  where
    loop cnt = do
      debugm "workerProc:top of loop"
      req <- liftIO $ atomically $ readTChan cin
      debugm $ "workerProc loop:got:" ++ show req
      case req of
        Cmd1 -> do
          liftIO $ atomically $ writeTChan cout (T.pack $ "wp cmd1:cnt=" ++ show cnt)
          loop (cnt + 1)
        Cmd2 -> do
          liftIO $ atomically $ writeTChan cout (T.pack $ "wp cmd2:cnt=" ++ show cnt)
          loop (cnt + 1)

-- ---------------------------------------------------------------------
