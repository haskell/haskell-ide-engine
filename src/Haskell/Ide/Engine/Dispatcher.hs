{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
module Haskell.Ide.Engine.Dispatcher
  (
    dispatcherP
  , DispatcherEnv(..)
  , ErrorHandler
  , CallbackHandler
  ) where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Monad.Trans.Free
import           Control.Monad.Except
import           Data.Foldable
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
import           Control.Lens
import qualified GhcMod.Monad as GM

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
dispatcherP inChan plugins ghcModOptions env errorHandler callbackHandler caps = do
  stateVarVar <- newEmptyMVar
  ideChan <- newTChanIO
  ghcChan <- newTChanIO
  let startState = IdeState emptyModuleCache Map.empty plugins Map.empty Nothing
      runGhcDisp = runIdeGhcM ghcModOptions caps startState $ do
        stateVar <- lift $ lift $ lift ask
        liftIO $ putMVar stateVarVar stateVar
        ghcDispatcher env errorHandler callbackHandler ghcChan
      runIdeDisp = do
        stateVar <- readMVar stateVarVar
        flip runReaderT stateVar $ getMTState $ flip runReaderT caps $
          ideDispatcher env errorHandler callbackHandler ideChan
      runMainDisp = mainDispatcher inChan ghcChan ideChan

  runGhcDisp `race_` runIdeDisp `race_` runMainDisp

mainDispatcher :: forall void m. TChan (PluginRequest m) -> TChan (GhcRequest m) -> TChan (IdeRequest m) -> IO void
mainDispatcher inChan ghcChan ideChan = forever $ do
  req <- atomically $ readTChan inChan
  atomically $ case req of
    Right r ->
      writeTChan ghcChan r
    Left r ->
      writeTChan ideChan r

ideDispatcher :: forall void m. DispatcherEnv -> ErrorHandler -> CallbackHandler m -> TChan (IdeRequest m) -> IdeM void
ideDispatcher env errorHandler callbackHandler pin = forever $ do
  debugm "ideDispatcher: top of loop"
  IdeRequest tn lid callback action <- liftIO $ atomically $ readTChan pin
  debugm $ "ideDispatcher: got request " ++ show tn ++ " with id: " ++ show lid
  handleAction lid $ fmap (liftIO . callbackHandler callback) action
  where handleAction :: J.LspId -> IdeResponseT (IdeM ()) -> IdeM ()
        handleAction lid action = do
          response <- runFreeT $ runIDErring $
            checkCancelled env lid *> action <* checkCancelled env lid
          case response of
            Pure result -> do
              completedReq env lid
              either (liftIO . handleError (errorHandler lid)) id result
            Free (IdeDefer fp cacheCb) -> queueAction fp $
              handleAction lid . either (\err -> ideError NoModuleAvailable err J.Null) (IDErring . ExceptT . cacheCb)

        queueAction :: FilePath -> (Either T.Text CachedModule -> IdeM ()) -> IdeM ()
        queueAction fp action = requestQueue . at fp . non' _Empty %= (action:)

ghcDispatcher :: forall void m. DispatcherEnv -> ErrorHandler -> CallbackHandler m -> TChan (GhcRequest m) -> GM.GhcModT IdeM void
ghcDispatcher env@DispatcherEnv{docVersionTVar} errorHandler callbackHandler pin = forever $ do
  debugm "ghcDispatcher: top of loop"
  GhcRequest tn context mver mid callback action <- liftIO $ atomically $ readTChan pin
  debugm $ "ghcDispatcher:got request " ++ show tn ++ " with id: " ++ show mid
  result <- runIDErring $ do
    for_ mid $ \lid -> do
      completedReq env lid
      checkCancelled env lid
    for_ mver $ \(uri, reqver) -> do
      curver <- liftIO $ atomically $ Map.lookup uri <$> readTVar docVersionTVar
      when (Just reqver /= curver) $
        ideError VersionMismatch "The request expects another version" J.Null
    let c = uriToFilePath <$> context
    when (c == Just Nothing) $ debugm "ghcDispatcher:Got malformed uri, running action with default context"
    runActionWithContext (join c) action
  liftIO $ case result of
    Right x -> callbackHandler callback x
    Left err -> case mid of
      Just lid -> handleError (errorHandler lid) err
      Nothing -> debugm $ "ghcDispatcher:Got error for a request: " ++ show err

handleError :: (J.ErrorCode -> T.Text -> a) -> IdeError -> a
handleError handler (IdeError code msg _) = handler (translate code) msg where
  translate RequestCancelled = J.RequestCancelled
  -- TODO: Ununknow error codes.
  translate NoModuleAvailable = J.UnknownErrorCode
  translate VersionMismatch = J.UnknownErrorCode
  translate _ = J.InternalError

checkCancelled :: MonadIO m => DispatcherEnv -> J.LspId -> IDErring m ()
checkCancelled env lid = do
  -- attempt to pop a corresponding cancel request
  cancelled <- liftIO $ atomically $ do
    c <- S.member lid <$> readTVar (cancelReqsTVar env)
    when c $ modifyTVar' (cancelReqsTVar env) (S.delete lid)
    return c
  when cancelled $ ideError RequestCancelled "" J.Null

completedReq :: MonadIO m => DispatcherEnv -> J.LspId -> m ()
completedReq env lid = liftIO $ atomically $ modifyTVar' (wipReqsTVar env) (S.delete lid)
