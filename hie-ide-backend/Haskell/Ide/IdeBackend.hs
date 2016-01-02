{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Haskell.Ide.IdeBackend where

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

typeCmd :: CommandFunc TypeInfo
typeCmd =
  CmdAsync $
  \resp _ctxs req ->
    case getParams (IdFile "file" :& IdPos "start_pos" :& IdPos "end_pos" :&
                    RNil)
                   req of
      Left err -> liftIO $ resp err
      Right (ParamFile filename :& ParamPos startPos :& ParamPos endPos :& RNil) ->
        do SubProcess cin cout _tid <- ensureProcessRunning filename
           liftIO $
             atomically $
             writeTChan cin
                        (Type filename startPos endPos)
           response <- liftIO $ atomically $ readTChan cout
           case response of
             TypeResp typeinfo -> liftIO $ resp (IdeResponseOk typeinfo)
             ErrorResp error' -> liftIO $ resp
               (IdeResponseError (IdeError PluginError error' Null))
      Right _ ->
        liftIO . resp $
        IdeResponseError
          (IdeError InternalError
                    "IdeBackendPlugin.typesCmd: ghc’s exhaustiveness checker is broken"
                    Null)

instance ExtensionClass AsyncPluginState where
  initialValue = APS Nothing

data AsyncPluginState = APS (Maybe SubProcess)

data WorkerCmd = Type T.Text Pos Pos deriving (Show)

data IdeBackendResponse = TypeResp TypeInfo | ErrorResp T.Text

data SubProcess = SubProcess
  { spChIn    :: TChan WorkerCmd
  , spChOut   :: TChan IdeBackendResponse
  , spProcess :: ThreadId
  }

ensureProcessRunning :: T.Text -> IdeM SubProcess
ensureProcessRunning filename = do
  (APS v) <- get -- from extensible state
  case v of
    Nothing -> do
      -- Get the required packagedbs from ghc-mod
      -- This won’t be necessary once we switch to one hie instance per project
      cradle' <- findCradle' (takeDirectory (T.unpack filename))
      pkgdbs <- gmeLocal (\(GhcModEnv opts _) -> GhcModEnv opts cradle') getPackageDbStack
      cin  <- liftIO $ atomically newTChan
      cout <- liftIO $ atomically newTChan
      tid  <- liftIO $ forkIO (workerProc pkgdbs cin cout)
      let v' = SubProcess cin cout tid
      put (APS (Just v')) -- into extensible state
      return v'
    Just v' -> return v'

logFunc :: LogFunc
logFunc _loc _source level logStr = logOtherm level (T.decodeUtf8 $ fromLogStr logStr)

workerProc :: [GhcPkgDb] -> TChan WorkerCmd -> TChan IdeBackendResponse -> IO ()
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

convPkgDb :: GhcPkgDb -> PackageDB
convPkgDb GlobalDb = GlobalPackageDB
convPkgDb UserDb = UserPackageDB
convPkgDb (PackageDb db) = SpecificPackageDB db

ignoreStatus :: Monad m => a -> m ()
ignoreStatus = const (return ())

handleTypeInfo :: IdeSession
               -> TChan IdeBackendResponse
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
