{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Haskell.Ide.IdeBackend
  (idebackendDescriptor
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Haskell.Ide.Engine.ExtensibleState
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           IdeSession
import           IdeSession.Util.Logger
import           Language.Haskell.GhcMod.Cradle
import           Language.Haskell.GhcMod.GhcPkg
import           Language.Haskell.GhcMod.Monad.Types hiding (liftIO)
import           Language.Haskell.GhcMod.Types hiding (liftIO,ModuleName)
import           System.FilePath
import           System.Log.FastLogger

idebackendDescriptor :: TaggedPluginDescriptor _
idebackendDescriptor = PluginDescriptor
  {
    pdUIShortName = "ide-backend"
  , pdUIOverview = "HIE plugin for ide-backend"
  , pdCommands =
         buildCommand typeCmd (Proxy :: Proxy "type") "type" [".hs"] (SCtxRegion :& RNil) RNil
      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- | Get the type for a region in a file
typeCmd :: CommandFunc TypeInfo
typeCmd =
  CmdSync $
  \_ctxs req ->
    case getParams (IdFile "file" :& IdPos "start_pos" :& IdPos "end_pos" :&
                    RNil)
                   req of
      Left err -> return err
      Right (ParamFile filename :& ParamPos startPos :& ParamPos endPos :& RNil) ->
        do SubProcess cin cout _tid <- ensureProcessRunning filename
           liftIO $
             atomically $
             writeTChan cin
                        (Type filename startPos endPos)
           response <- liftIO $ atomically $ readTChan cout
           case response of
             TypeResp typeinfo -> return (IdeResponseOk typeinfo)
             ErrorResp error' ->
               return (IdeResponseError (IdeError PluginError error' Null))
      Right _ ->
        return (IdeResponseError
                  (IdeError InternalError
                            "IdeBackendPlugin.typesCmd: ghc’s exhaustiveness checker is broken"
                            Null))

instance ExtensionClass AsyncPluginState where
  initialValue = APS Nothing

-- | Holds the worker process needed to cache the `IdeSession`
data AsyncPluginState = APS (Maybe SubProcess)

-- | Commands send to the worker process
data WorkerCmd = Type T.Text Pos Pos deriving (Show)

-- | Responses from the worker process
data WorkerResponse = TypeResp TypeInfo | ErrorResp T.Text

-- | The state for a worker process, consisting of two communicating
-- channels and the `ThreadId`
data SubProcess = SubProcess
  { spChIn    :: TChan WorkerCmd
  , spChOut   :: TChan WorkerResponse
  , spProcess :: ThreadId
  }

-- | Try to find an already running process or start a new one if it
-- doesn’t already exist
ensureProcessRunning :: T.Text -> IdeM SubProcess
ensureProcessRunning filename =
  do (APS v) <- get -- from extensible state
     case v of
       Nothing ->
         do
            -- Get the required packagedbs from ghc-mod
            -- This won’t be necessary once we switch to one hie instance per project
            cradle' <- findCradle' (takeDirectory (T.unpack filename))
            pkgdbs <-
              gmeLocal (\(GhcModEnv opts _) -> GhcModEnv opts cradle') getPackageDbStack
            cin <- liftIO $ atomically newTChan
            cout <- liftIO $ atomically newTChan
            tid <- liftIO $ forkIO (workerProc pkgdbs cin cout)
            let v' =
                  SubProcess {spChIn = cin
                             ,spChOut = cout
                             ,spProcess = tid}
            put (APS (Just v')) -- into extensible state
            return v'
       Just v' -> return v'

-- | Log function to get ide-backend to use our logger
logFunc :: LogFunc
logFunc _loc _source level logStr =
  logOtherm level (T.decodeUtf8 $ fromLogStr logStr)

-- | Long running worker process responsible for processing the commands
workerProc :: [GhcPkgDb] -> TChan WorkerCmd -> TChan WorkerResponse -> IO ()
workerProc pkgdbs cin cout =
  do session <-
       initSessionWithCallbacks
         (IdeCallbacks logFunc)
         (defaultSessionInitParams {sessionInitTargets = TargetsInclude []})
         (defaultSessionConfig {configLocalWorkingDir = Nothing
                               ,configLog = debugm
                               ,configPackageDBStack = (GlobalPackageDB:) $ map convPkgDb pkgdbs})
     updateSession session
                   (updateCodeGeneration True)
                   (debugm . show)
     let loop :: Int -> IO ()
         loop cnt =
           do debugm "workerProc:top of loop"
              req <- liftIO $ atomically $ readTChan cin
              debugm $ "workerProc loop:got:" ++ show req
              case req of
                Type file startPos endPos ->
                  do liftIO $
                       handleTypeInfo session cout file startPos endPos
                     loop (cnt + 1)
     loop 1

-- | Convert the package database from ghc-mod’s representation to cabal’s
-- representation
convPkgDb :: GhcPkgDb -> PackageDB
convPkgDb GlobalDb = GlobalPackageDB
convPkgDb UserDb = UserPackageDB
convPkgDb (PackageDb db) = SpecificPackageDB db

-- | Find the type for a region in a file. Add the supplied file to
-- the session targets.
handleTypeInfo :: IdeSession
               -> TChan WorkerResponse
               -> T.Text
               -> Pos
               -> Pos
               -> IO ()
handleTypeInfo session cout file (startLine,startCol) (endLine,endCol) =
  do updateSession session
                   (updateTargets (TargetsInclude . pure $ T.unpack file))
                   (debugm . show)
     errors <- getSourceErrors session
     case errors of
       (_:_) -> atomically . writeTChan cout $ ErrorResp (T.pack $ show errors)
       [] ->
         do filemap <- getFileMap session
            expTypes <- getExpTypes session
            case filemap (T.unpack file) of
              Nothing ->
                atomically . writeTChan cout $ ErrorResp "No module found"
              Just mod' ->
                case expTypes (moduleName mod')
                              SourceSpan {spanFilePath = T.unpack file
                                         ,spanFromLine = startLine
                                         ,spanToLine = endLine
                                         ,spanFromColumn = startCol
                                         ,spanToColumn = endCol} of
                  ts ->
                    atomically . writeTChan cout . TypeResp . TypeInfo $
                    map toTypeResult ts
  where toTypeResult
          :: (SourceSpan,T.Text) -> TypeResult
        toTypeResult (SourceSpan{..},t) =
          TypeResult {trStart = (spanFromLine,spanFromColumn)
                     ,trEnd = (spanToLine,spanToColumn)
                     ,trText = t}
