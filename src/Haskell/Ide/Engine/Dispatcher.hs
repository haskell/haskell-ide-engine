{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE LambdaCase                #-}
module Haskell.Ide.Engine.Dispatcher
  (
    dispatcherP
  , DispatcherEnv(..)
  , ErrorHandler
  , CallbackHandler
  ) where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.Aeson                              as J
import qualified Data.Text                               as T
import qualified Data.Map                                as Map
import qualified Data.Set                                as S
import qualified GhcMod.Types                            as GM
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.Engine.Monad
import qualified Language.Haskell.LSP.Types              as J
import qualified Language.Haskell.LSP.Types.Capabilities as J

data DispatcherEnv = DispatcherEnv
  { cancelReqsTVar     :: !(TVar (S.Set J.LspId))
  , wipReqsTVar        :: !(TVar (S.Set J.LspId))
  , docVersionTVar     :: !(TVar (Map.Map Uri Int))
  }

-- | A handler for any errors that the dispatcher may encounter.
type ErrorHandler = J.LspId -> J.ErrorCode -> T.Text -> IO ()
-- | A handler to run the requests' callback in your monad of choosing.
type CallbackHandler m = forall a. RequestCallback m a -> a -> IO ()

dispatcherP :: forall m. TChan (PluginRequest m)
            -> IdePlugins
            -> GM.Options
            -> DispatcherEnv
            -> ErrorHandler
            -> CallbackHandler m
            -> J.ClientCapabilities
            -> IO ()
dispatcherP inChan plugins ghcModOptions env errorHandler callbackHandler caps =
  void $ runIdeGhcM ghcModOptions caps (IdeState emptyModuleCache Map.empty plugins Map.empty Nothing) $ do
      stateVar <- lift $ lift $ lift ask
      gchan <- liftIO $ do
        ghcChan <- newTChanIO
        ideChan <- newTChanIO

        _ <- forkIO $ mainDispatcher inChan ghcChan ideChan
        _ <- forkIO $ flip runReaderT stateVar $
                        flip runReaderT caps $ 
                          ideDispatcher env errorHandler callbackHandler ideChan

        -- TODO: Causes STM deadlock?
        --mainThread <- myThreadId
        --let exceptionHandler e = do
        --      --TODO: Use async's link
        --      errorm $ show (e :: SomeException)
        --      throwTo mainThread e
        --    ideLoop = handle exceptionHandler
        --                     (runReaderT (runReaderT  (ideDispatcher env errorHandler callbackHandler ideChan) caps) stateVar)
        --_ <- forkIO ideLoop
        return ghcChan
      ghcDispatcher env errorHandler callbackHandler gchan

mainDispatcher :: forall void m. TChan (PluginRequest m) -> TChan (GhcRequest m) -> TChan (IdeRequest m) -> IO void
mainDispatcher inChan ghcChan ideChan = forever $ do
  req <- atomically $ readTChan inChan
  case req of
    Right r ->
      atomically $ writeTChan ghcChan r
    Left r ->
      atomically $ writeTChan ideChan r

ideDispatcher :: forall void m. DispatcherEnv -> ErrorHandler -> CallbackHandler m -> TChan (IdeRequest m) -> IdeM void
ideDispatcher env errorHandler callbackHandler pin = forever $ do
  debugm "ideDispatcher: top of loop"
  (IdeRequest tn lid callback action) <- liftIO $ atomically $ readTChan pin
  debugm $ "ideDispatcher: got request " ++ show tn ++ " with id: " ++ show lid
  checkCancelled env lid errorHandler $ do
    response <- action
    handleResponse lid callback response

  where handleResponse lid callback response =
          -- Need to check cancellation twice since cancellation
          -- request might have come in during the action
          checkCancelled env lid errorHandler $ case response of
            IdeResponseResult (IdeResultOk x) -> liftIO $ do
              completedReq env lid
              callbackHandler callback x
            IdeResponseResult (IdeResultFail (IdeError code msg _)) -> liftIO $ do
              completedReq env lid
              case code of
                -- TODO: This isn't actually an internal error
                NoModuleAvailable -> errorHandler lid J.InternalError msg
                _ -> errorHandler lid J.InternalError msg
            IdeResponseDeferred fp cacheCb -> handleDeferred lid fp cacheCb callback

        handleDeferred lid fp cacheCb actualCb = queueAction fp $ \case
          Right cm -> do
            cacheResponse <- cacheCb cm
            handleResponse lid actualCb cacheResponse
          Left err ->
            handleResponse lid actualCb (IdeResponseFail (IdeError NoModuleAvailable err J.Null))

        queueAction :: FilePath -> (Either T.Text CachedModule -> IdeM ()) -> IdeM ()
        queueAction fp action =
          lift $ modifyMTState $ \s ->
            let oldQueue = requestQueue s
                -- add to existing queue if possible
                update Nothing = [action]
                update (Just x) = action : x
                newQueue = Map.alter (Just . update) fp oldQueue
            in s { requestQueue = newQueue }

ghcDispatcher :: forall void m. DispatcherEnv -> ErrorHandler -> CallbackHandler m -> TChan (GhcRequest m) -> IdeGhcM void
ghcDispatcher env@DispatcherEnv{docVersionTVar} errorHandler callbackHandler pin = forever $ do
  debugm "ghcDispatcher: top of loop"
  (GhcRequest tn context mver mid callback action) <- liftIO $ atomically $ readTChan pin
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
          IdeResultFail err@(IdeError code msg _) ->
            case mid of
              Just lid -> case code of
                NoModuleAvailable -> errorHandler lid J.ParseError msg
                _ -> errorHandler lid J.InternalError msg
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
