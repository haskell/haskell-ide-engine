{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Haskell.Ide.Engine.Dispatcher
  (
    ideDispatcher
  , ghcDispatcher
  , DispatcherEnv(..)
  , ErrorHandler
  , CallbackHandler
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.Text                               as T
import qualified Data.Map                                as Map
import qualified Data.Set                                as S

import qualified Language.Haskell.LSP.Types              as J
import qualified Language.Haskell.LSP.Types.Capabilities as J

import qualified Haskell.Ide.Engine.Channel              as Channel
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Types

data DispatcherEnv = DispatcherEnv
  { cancelReqsTVar     :: !(TVar (S.Set J.LspId))
  , wipReqsTVar        :: !(TVar (S.Set J.LspId))
  , docVersionTVar     :: !(TVar (Map.Map Uri Int))
  }

-- | A handler for any errors that the dispatcher may encounter.
type ErrorHandler = J.LspId -> J.ErrorCode -> T.Text -> IO ()
-- | A handler to run the requests' callback in your monad of choosing.
type CallbackHandler m = forall a. RequestCallback m a -> a -> IO ()


ideDispatcher :: forall void m. TVar IdeState -> J.ClientCapabilities
              -> DispatcherEnv -> ErrorHandler -> CallbackHandler m
              -> Channel.OutChan (IdeRequest m) -> IO void
ideDispatcher stateVar caps env errorHandler callbackHandler pin =
  -- TODO: AZ run a single ReaderT, with a composite R.
  flip runReaderT stateVar $ flip runReaderT caps $ forever $ do
    debugm "ideDispatcher: top of loop"
    (IdeRequest tn lid callback action) <- liftIO $ Channel.readChan pin
    debugm $ "ideDispatcher: got request " ++ show tn ++ " with id: " ++ show lid

    iterT queueDeferred $
      checkCancelled env lid errorHandler $ do
        result <- action
        checkCancelled env lid errorHandler $ liftIO $ do
          completedReq env lid
          case result of
            IdeResultOk x -> callbackHandler callback x
            IdeResultFail (IdeError _ msg _) -> errorHandler lid J.InternalError msg

  where queueDeferred (Defer fp cacheCb) =
          lift $ modifyMTState $ \s ->
            let oldQueue = requestQueue s
                -- add to existing queue if possible
                update Nothing = [cacheCb]
                update (Just x) = cacheCb : x
                newQueue = Map.alter (Just . update) fp oldQueue
            in s { requestQueue = newQueue }

ghcDispatcher :: forall void m. DispatcherEnv -> ErrorHandler -> CallbackHandler m -> Channel.OutChan (GhcRequest m) -> IdeGhcM void
ghcDispatcher env@DispatcherEnv{docVersionTVar} errorHandler callbackHandler pin = forever $ do
  debugm "ghcDispatcher: top of loop"
  (GhcRequest tn context mver mid callback action) <- liftIO $  Channel.readChan pin
  debugm $ "ghcDispatcher:got request " ++ show tn ++ " with id: " ++ show mid

  let runner = case context of
        Nothing -> runActionWithContext Nothing
        Just uri -> case uriToFilePath uri of
          Just fp -> runActionWithContext (Just fp)
          Nothing -> \act -> do
            debugm "ghcDispatcher:Got malformed uri, running action with default context"
            runActionWithContext Nothing act

  let runWithCallback = do
        result <- runner action
        liftIO $ case result of
          IdeResultOk x -> callbackHandler callback x
          IdeResultFail err@(IdeError _ msg _) ->
            case mid of
              Just lid -> errorHandler lid J.InternalError msg
              Nothing -> debugm $ "ghcDispatcher:Got error for a request: " ++ show err

  let runIfVersionMatch = case mver of
        Nothing -> runWithCallback
        Just (uri, reqver) -> do
          curver <- liftIO $ atomically $ Map.lookup uri <$> readTVar docVersionTVar
          if Just reqver /= curver then
            debugm "ghcDispatcher:not processing request as it is for old version"
          else do
            debugm "ghcDispatcher:Processing request as version matches"
            runWithCallback

  case mid of
    Nothing -> runIfVersionMatch
    Just lid -> checkCancelled env lid errorHandler $ do
      liftIO $ completedReq env lid
      runIfVersionMatch

checkCancelled :: MonadIO m => DispatcherEnv -> J.LspId -> ErrorHandler -> m () -> m ()
checkCancelled env lid errorHandler callback = do
  cancelled <- liftIO $ atomically isCancelled
  if cancelled
    then liftIO $ do
      -- remove from cancelled and wip list
      atomically $ modifyTVar' (cancelReqsTVar env) (S.delete lid)
      completedReq env lid
      errorHandler lid J.RequestCancelled ""
    else callback
  where isCancelled = S.member lid <$> readTVar (cancelReqsTVar env)

completedReq :: DispatcherEnv -> J.LspId -> IO ()
completedReq env lid = atomically $ modifyTVar' (wipReqsTVar env) (S.delete lid)
