{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NamedFieldPuns            #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.Map                              as Map
import qualified Data.Set                              as S
import qualified GhcMod.ModuleLoader                   as GM
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

data DispatcherEnv = DispatcherEnv
  { cancelReqsTVar     :: !(TVar (S.Set J.LspId))
  , wipReqsTVar        :: !(TVar (S.Set J.LspId))
  , docVersionTVar     :: !(TVar (Map.Map Uri Int))
  }

dispatcherP :: forall void. DispatcherEnv -> TChan PluginRequest -> IdeM void
dispatcherP env inChan = do
  stateVar <- lift . lift $ ask
  ichan <- liftIO $ do
    ideChan <- newTChanIO
    asyncChan <- newTChanIO
    forkIO $ mainDispatcher inChan ideChan asyncChan
    forkIO $ runReaderT (asyncDispatcher env asyncChan) stateVar
    return ideChan
  ideDispatcher env ichan

mainDispatcher :: forall void. TChan PluginRequest -> TChan PluginRequest -> TChan PluginRequest -> IO void
mainDispatcher inChan ideChan asyncChan = forever $ do
  req <- atomically $ readTChan inChan
  case req of
    PReq{} ->
      atomically $ writeTChan ideChan req
    PureReq{} ->
      atomically $ writeTChan asyncChan req

asyncDispatcher :: forall void. DispatcherEnv -> TChan PluginRequest -> AsyncM void
asyncDispatcher env pin = forever $ do
  (PureReq lid callback action) <- liftIO $ atomically $ readTChan pin
  debugm $ "got request with id: " ++ show lid
  cancelled <- liftIO $ atomically $ isCancelled env lid
  unless cancelled $ do
    res <- action
    liftIO $ callback res

ideDispatcher :: forall void. DispatcherEnv -> TChan PluginRequest -> IdeM void
ideDispatcher env@DispatcherEnv{docVersionTVar} pin = forever $ do
  debugm "ideDispatcher: top of loop"
  (PReq context mver mid callback action) <- liftIO $ atomically $ readTChan pin
  debugm $ "got request with id: " ++ show mid

  let runner = case context of
        Nothing -> GM.runActionWithContext Nothing
        Just uri -> case uriToFilePath uri of
          Just fp -> GM.runActionWithContext (Just fp)
          Nothing -> \act -> do
            debugm "Got malformed uri, running action with default context"
            GM.runActionWithContext Nothing act

  let runWithCallback = do
        r <- runner action
        liftIO $ callback r

  let runIfVersionMatch = case mver of
        Nothing -> runWithCallback
        Just (uri, reqver) -> do
          curver <- liftIO $ atomically $ Map.lookup uri <$> readTVar docVersionTVar
          if Just reqver /= curver then
            debugm "not processing request as it is for old version"
          else do
            debugm "Processing request as version matches"
            runWithCallback

  case mid of
    Nothing -> runIfVersionMatch
    Just lid -> do
      cancelled <- liftIO $ atomically $ isCancelled env lid
      if cancelled
      then
        debugm $ "cancelling request: " ++ show lid
      else do
        debugm $ "processing request: " ++ show lid
        runIfVersionMatch

-- Deletes the request from both wipReqs and cancelReqs
isCancelled :: DispatcherEnv -> J.LspId -> STM Bool
isCancelled DispatcherEnv{cancelReqsTVar,wipReqsTVar} lid = do
  modifyTVar' wipReqsTVar (S.delete lid)
  creqs <- readTVar cancelReqsTVar
  modifyTVar' cancelReqsTVar (S.delete lid)
  return $ S.member lid creqs
