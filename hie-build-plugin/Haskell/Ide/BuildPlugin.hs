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
import           Haskell.Ide.Engine.ExtensibleState
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Monoid
import qualified Data.Map as Map
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO (openFile, hClose, IOMode(..))
import System.IO.Error

import Distribution.Simple.Setup (defaultDistPref)
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils (findPackageDesc, withFileContents)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity as Verb

import Data.Yaml

-- ---------------------------------------------------------------------

buildPluginDescriptor :: TaggedPluginDescriptor _
buildPluginDescriptor = PluginDescriptor
  {
    pdUIShortName = "Build plugin"
  , pdUIOverview = "A HIE plugin for building cabal/stack packages"
  , pdCommands =
        buildCommand listTargets (Proxy :: Proxy "listTargets")
            "Given a directory with stack/cabal project lists all its targets"
            [] (SCtxNone :& RNil)
            (  SParamDesc (Proxy :: Proxy "directory") (Proxy :: Proxy "Directory to search for project file") SPtFile SRequired
            :& SParamDesc (Proxy :: Proxy "type") (Proxy :: Proxy "Project type: \"stack\" or \"cabal\"") SPtText SRequired
            :& RNil)
      :& buildCommand addTarget (Proxy :: Proxy "addTarget") "Add a new target to the cabal file" [] (SCtxNone :& RNil)
            (  SParamDesc (Proxy :: Proxy "file") (Proxy :: Proxy "Path to the .cabal file") SPtFile SRequired
            :& SParamDesc (Proxy :: Proxy "name") (Proxy :: Proxy "Name of the new target") SPtText SRequired
            :& SParamDesc (Proxy :: Proxy "type") (Proxy :: Proxy "executable/library") SPtText SRequired
            :& RNil)
      :& buildCommand (longRunningCmdSync Cmd2) (Proxy :: Proxy "cmd2") "Long running synchronous command" [] (SCtxNone :& RNil) RNil
      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

listTargets :: CommandFunc Object
listTargets = CmdSync $ \ctx req -> do
    case getParams (IdFile "directory" :& IdText "type" :& RNil) req of
        Left err -> return err
        Right (ParamFile dir :& ParamText type_ :& RNil) -> do
            let buildDir = maybe defaultDistPref (\(ParamTextP v) -> T.unpack v) $ Map.lookup "buildDir" (ideParams req)
            targets <- liftIO $ listTargets' type_ buildDir (T.unpack dir)
            let (Object ret) = object ["res" .= toJSON targets]
            return $ IdeResponseOk ret

data StackYaml = StackYaml [StackPackage]
data StackPackage = LocalOrHTTPPackage { stackPackageName :: String }
                  | Repository

isLocal (LocalOrHTTPPackage _) = True
isLocal _ = False

instance FromJSON StackYaml where
  parseJSON (Object o) = StackYaml <$>
    o .: "packages"

instance FromJSON StackPackage where
  parseJSON (Object o) = pure Repository
  parseJSON (String s) = pure $ LocalOrHTTPPackage (T.unpack s)

withBinaryFileContents name act =
  Exception.bracket (openFile name ReadMode) hClose
                    (\hnd -> B.hGetContents hnd >>= act)

listTargets' type_ buildDir dir = do
  case type_ of
    "stack" -> withBinaryFileContents (dir </> "stack.yaml") $ \contents -> do
      let (Just (StackYaml stackYaml)) = decode contents
          stackPackageDirs = map stackPackageName $ filter isLocal stackYaml
      concat <$> mapM (listTargets' "cabal" buildDir) stackPackageDirs
    "cabal" -> do
      cabalFile <- findPackageDesc dir
      case cabalFile of
        Left _ -> return []
        Right cabalFile -> withFileContents cabalFile $ \contents ->
          case parsePackageDescription contents of
            ParseFailed _ -> return []
            ParseOk _ genPkgDescr -> do
              maybeLBI <- tryGetPersistBuildConfig buildDir
              case maybeLBI of
                Right lbi -> return $ listCabalTargets $ localPkgDescr lbi
                Left _ -> return $ listCabalTargets $ flattenPackageDescription genPkgDescr

data Target = ExecutableTarget String
            | LibraryTarget String
            | TestTarget String

instance ToJSON Target where
  toJSON (ExecutableTarget n) = object ["type" .= ("executable" :: T.Text), "name" .= n]
  toJSON (LibraryTarget n) = object ["type" .= ("library" :: T.Text), "name" .= n]
  toJSON (TestTarget n) = object ["type" .= ("test" :: T.Text), "name" .= n]

listCabalTargets pkgDescr = execs ++ tests ++ maybeLibrary
  where
    execs = map (ExecutableTarget . exeName) $ executables pkgDescr
    maybeLibrary = map (const $ LibraryTarget $ unPackageName $ pkgName $ package pkgDescr) $ maybeToList $ library pkgDescr
    tests = map (TestTarget . testName) $ testSuites pkgDescr

addTarget = CmdSync $ \ctx req -> do
  let args = (,,) <$> Map.lookup "file" (ideParams req)
                  <*> Map.lookup "name" (ideParams req)
                  <*> Map.lookup "type" (ideParams req)
  case args of
      Nothing -> return $ missingParameter "file, name or type"
      Just (ParamFileP file, ParamTextP name, ParamTextP _type) -> do
          res <- liftIO $ addTarget' (T.unpack file) (T.unpack name) (T.unpack _type)
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
