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
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Types            as J
import           System.Directory

data DispatcherEnv = DispatcherEnv
  { cancelReqsTVar     :: !(TVar (S.Set J.LspId))
  , wipReqsTVar        :: !(TVar (S.Set J.LspId))
  , docVersionTVar     :: !(TVar (Map.Map Uri Int))
  }

dispatcherP :: forall void. DispatcherEnv -> TChan PluginRequest -> IdeGhcM void
dispatcherP env inChan = do
  stateVar <- lift . lift $ ask
  gchan <- liftIO $ do
    ghcChan <- newTChanIO
    ideChan <- newTChanIO
    _ <- forkIO $ mainDispatcher inChan ghcChan ideChan
    _ <- forkIO $ runReaderT (ideDispatcher env ideChan) stateVar
    return ghcChan
  ghcDispatcher env gchan

mainDispatcher :: forall void. TChan PluginRequest -> TChan GhcRequest -> TChan IdeRequest -> IO void
mainDispatcher inChan ghcChan ideChan = forever $ do
  req <- atomically $ readTChan inChan
  case req of
    Right r ->
      atomically $ writeTChan ghcChan r
    Left r ->
      atomically $ writeTChan ideChan r

ideDispatcher :: forall void. DispatcherEnv -> TChan IdeRequest -> IdeM void
ideDispatcher env pin = forever $ do
  debugm "ideDispatcher: top of loop"
  (IdeRequest lid callback action) <- liftIO $ atomically $ readTChan pin
  debugm $ "ideDispatcher:got request with id: " ++ show lid
  cancelled <- liftIO $ atomically $ isCancelled env lid
  unless cancelled $ do
    response <- action
    case response of
      IdeResponseResult result -> liftIO $ callback result
      IdeResponseDeferred fp cacheCb -> handleDeferred fp cacheCb callback
      
  where handleDeferred :: FilePath -> (CachedModule -> IdeGhcM (IdeResponse a)) -> (IdeResult a -> IO ()) -> IdeM ()
        handleDeferred fp cacheCb actualCb = queueAction fp $ \cm -> do
          cacheResponse <- cacheCb cm
          case cacheResponse of
            IdeResponseDeferred fp2 cacheCb2 -> liftToGhc $ handleDeferred fp2 cacheCb2 actualCb
            IdeResponseResult result -> liftIO $ actualCb result
    
        queueAction :: FilePath -> (CachedModule -> IdeGhcM ()) -> IdeM ()
        queueAction fp action = do
          fp' <- liftIO $ canonicalizePath fp
          modifyMTS $ \s ->
            let oldQueue = actionQueue s
                -- add to existing queue if possible
                newQueue = if Map.member fp' oldQueue
                  then Map.update (Just . (action :)) fp oldQueue
                  else Map.insert fp [action] oldQueue
            in s { actionQueue = newQueue }

ghcDispatcher :: forall void. DispatcherEnv -> TChan GhcRequest -> IdeGhcM void
ghcDispatcher env@DispatcherEnv{docVersionTVar} pin = forever $ do
  debugm "ghcDispatcher: top of loop"
  (GhcRequest context mver mid callback action) <- liftIO $ atomically $ readTChan pin
  debugm $ "got request with id: " ++ show mid

  let runner = case context of
        Nothing -> runActionWithContext Nothing
        Just uri -> case uriToFilePath uri of
          Just fp -> runActionWithContext (Just fp)
          Nothing -> \act -> do
            debugm "Got malformed uri, running action with default context"
            runActionWithContext Nothing act

  let runWithCallback = do
        result <- runner action
        liftIO $ callback result

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
