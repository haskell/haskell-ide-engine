{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
module Haskell.Ide.Engine.Scheduler
  ( Scheduler
  , DocUpdate
  , ErrorHandler
  , CallbackHandler
  , HasScheduler(..)
  , newScheduler
  , runScheduler
  , sendRequest
  , cancelRequest
  , makeRequest
  , updateDocumentRequest
  , updateDocument
  )
where

import           Control.Concurrent.Async
import           GHC.Conc
import qualified Control.Concurrent.STM        as STM
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Control.Monad.Reader.Class     ( ask
                                                , MonadReader
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           HIE.Bios.Types
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.Types    as J
import           GhcMonad

import qualified HIE.Bios.Types as Bios
import           Haskell.Ide.Engine.GhcModuleCache
import qualified Haskell.Ide.Engine.Cradle as Bios
import           Haskell.Ide.Engine.Config
import qualified Haskell.Ide.Engine.Channel    as Channel
import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes

import           Debug.Trace


-- | A Scheduler is a coordinator between the two main processes the ide engine uses
-- for responding to users requests. It accepts all of the requests and dispatches
-- them accordingly. One process accepts requests that require a GHC session such as
-- parsing, type checking and generating error diagnostics, whereas another process deals
-- with IDE features such as code navigation, code completion and symbol information.
--
-- It needs to be run using the 'runScheduler' function after being created in
-- order to start dispatching requests.
--
-- Schedulers are parameterized in the monad of your choosing, which is the monad where
-- request handlers and error handlers will run.
data Scheduler m = Scheduler
 { plugins :: IdePlugins
   -- ^ The list of plugins that will be used for responding to requests

 , biosOpts :: CradleOpts
   -- ^ Options for the hie-bios cradle finding

 , requestsToCancel :: STM.TVar (Set.Set J.LspId)
   -- ^ The request IDs that were canceled by the client. This causes requests to
   -- not be dispatched or aborted if they are already in progress.

 , requestsInProgress :: STM.TVar (Set.Set J.LspId)
   -- ^ Requests IDs that have already been dispatched. Currently this is only used to keep
   -- @requestsToCancel@ bounded. We only insert IDs into the cancel list if the same LspId is
   -- also present in this variable.

 , documentVersions :: STM.TVar (Map.Map Uri Int)
   -- ^ A Map containing document file paths with their respective current version. This is used
   -- to prevent certain requests from being processed if the current version is more recent than
   -- the version the request is for.

 , ideChan :: (Channel.InChan (IdeRequest m), Channel.OutChan (IdeRequest m))
   -- ^ Holds the reading and writing ends of the channel used to dispatch Ide requests

 , ghcChan :: (Channel.InChan (GhcRequest m), Channel.OutChan (GhcRequest m))
   -- ^ Holds the reading and writing ends of the channel used to dispatch Ghc requests
 }

-- ^ A pair representing the document file path and a new version to store for it.
type DocUpdate = (Uri, Int)


class HasScheduler a m where
  getScheduler :: a -> Scheduler m

-- | Create a new scheduler parameterized with the monad of your choosing.
-- This is the monad where the handler for requests and handler for errors will run.
--
-- Once created, the scheduler needs to be run using 'runScheduler'
newScheduler
  :: IdePlugins
     -- ^ The list of plugins that will be used for responding to requests
  -> CradleOpts
   -- ^ Options for the bios session. Since we only keep a single bios option record.
  -> IO (Scheduler m)
newScheduler plugins cradleOpts = do
  cancelTVar  <- STM.atomically $ STM.newTVar Set.empty
  wipTVar     <- STM.atomically $ STM.newTVar Set.empty
  versionTVar <- STM.atomically $ STM.newTVar Map.empty
  ideChan     <- Channel.newChan
  ghcChan     <- Channel.newChan
  return $ Scheduler
    { plugins            = plugins
    , biosOpts           = cradleOpts
    , requestsToCancel   = cancelTVar
    , requestsInProgress = wipTVar
    , documentVersions   = versionTVar
    , ideChan            = ideChan
    , ghcChan            = ghcChan
    }

-- | A handler for any errors that the dispatcher may encounter.
type ErrorHandler = Maybe J.LspId -> J.ErrorCode -> T.Text -> IO ()

-- | A handler to run the requests' callback in your monad of choosing.
type CallbackHandler m = forall a. RequestCallback m a -> a -> IO ()


-- | Runs the given scheduler. This is meant to run in a separate thread and
-- the thread should be kept alive as long as you need requests to be dispatched.
runScheduler
  :: forall m
   . Scheduler m
     -- ^ The scheduler to run.
  -> ErrorHandler
     -- ^ A handler for any errors that the dispatcher may encounter.
  -> CallbackHandler m
     -- ^ A handler to run the requests' callback in your monad of choosing.
  -> Core.LspFuncs Config
      -- ^ The LspFuncs provided by haskell-lsp.
  -> PublishDiagnostics
  -> Maybe Bios.Cradle
     -- ^ Context in which the ghc thread is executed.
     -- Neccessary to obtain the libdir, for example.
  -> IO ()
runScheduler Scheduler {..} errorHandler callbackHandler lf pubDiags  mcradle = do
  let dEnv = DispatcherEnv
        { cancelReqsTVar = requestsToCancel
        , wipReqsTVar    = requestsInProgress
        , docVersionTVar = documentVersions
        }

  let (_, ghcChanOut) = ghcChan
      (_, ideChanOut) = ideChan

  let initialState = IdeState emptyModuleCache Map.empty Map.empty Nothing

  stateVar <- STM.newTVarIO initialState

  mlibdir <- case mcradle of
    Nothing -> return Nothing
    Just crdl -> Bios.getProjectGhcLibDir crdl

  let runGhcDisp = runIdeGhcM mlibdir plugins lf stateVar $
                    ghcDispatcher dEnv errorHandler pubDiags callbackHandler ghcChanOut
      runIdeDisp = runIdeM plugins lf stateVar $
                    ideDispatcher dEnv errorHandler callbackHandler ideChanOut


  withAsync runGhcDisp $ \a ->
    withAsync runIdeDisp $ \b -> do
      flip labelThread "ghc" $ asyncThreadId a
      flip labelThread "ide" $ asyncThreadId b
      waitEither_ a b



-- | Sends a request to the scheduler so that it can be dispatched to the handler
-- function. Certain requests may never be dispatched if they get canceled
-- by the client by the time they reach the head of the queue.
--
-- If a 'DocUpdate' is provided, the version for the given document is updated
-- before the request is queued. This may cause other requests to never be processed if
-- the current version of the document differs from the version the request is meant for.
sendRequest
  :: forall m
   . Scheduler m
    -- ^ The scheduler to send the request to.
   -> PluginRequest m
    -- ^ The request to dispatch.
  -> IO ()
sendRequest Scheduler {..} req = do
  let (ghcChanIn, _) = ghcChan
      (ideChanIn, _) = ideChan

  case req of
    Right ghcRequest@GhcRequest { pinLspReqId = Nothing } ->
      Channel.writeChan ghcChanIn ghcRequest

    Right ghcRequest@GhcRequest { pinLspReqId = Just lid } ->
      STM.atomically $ do
        STM.modifyTVar requestsInProgress (Set.insert lid)
        Channel.writeChanSTM ghcChanIn ghcRequest

    Left ideRequest@IdeRequest { pureReqId } -> STM.atomically $ do
      STM.modifyTVar requestsInProgress (Set.insert pureReqId)
      Channel.writeChanSTM ideChanIn ideRequest

-- | Cancels a request previously sent to the given scheduler. This causes the
-- request with the same LspId to never be dispatched, or aborted if already in progress.
cancelRequest :: forall m . Scheduler m -> J.LspId -> IO ()
cancelRequest Scheduler { requestsToCancel, requestsInProgress } lid =
  STM.atomically $ do
    wip <- STM.readTVar requestsInProgress
    when (Set.member lid wip)
      $ STM.modifyTVar' requestsToCancel (Set.insert lid)

-- | Sends a single request to the scheduler so it can be be processed
-- asynchronously.
makeRequest
  :: (MonadReader env m, MonadIO m, HasScheduler env m2)
  => PluginRequest m2
  -> m ()
makeRequest req = do
  env <- ask
  liftIO $ sendRequest (getScheduler env) req

-- | Updates the version of a document and then sends the request to be processed
-- asynchronously.
updateDocumentRequest
  :: (MonadReader env m, MonadIO m, HasScheduler env m2)
  => Uri
  -> Int
  -> PluginRequest m2
  -> m ()
updateDocumentRequest uri ver req = do
  env <- ask
  let sched = (getScheduler env)
  liftIO $ do
    updateDocument sched uri ver
    sendRequest sched req

-- | Updates the version of a document and then sends the request to be processed
-- asynchronously.
updateDocument
  :: Scheduler a
  -> Uri
  -> Int
  -> IO ()
updateDocument sched uri ver =
  STM.atomically $ STM.modifyTVar' (documentVersions sched) (Map.insert uri ver)

-------------------------------------------------------------------------------
-- Dispatcher
-------------------------------------------------------------------------------

data DispatcherEnv = DispatcherEnv
  { cancelReqsTVar     :: !(STM.TVar (Set.Set J.LspId))
  , wipReqsTVar        :: !(STM.TVar (Set.Set J.LspId))
  , docVersionTVar     :: !(STM.TVar (Map.Map Uri Int))
  }

-- | Processes requests published in the channel and runs the give callback
-- or error handler as appropriate. Requests will not be processed if they
-- were cancelled before. If already in progress and then cancelled, the callback
-- will not be invoked in that case.
-- Meant to be run in a separate thread and be kept alive.
ideDispatcher
  :: forall void m
   . DispatcherEnv
     -- ^ A structure focusing on the mutable variables the dispatcher
     -- is allowed to modify.
  -> ErrorHandler
     -- ^ Callback to run in case of errors.
  -> CallbackHandler m
     -- ^ Callback to run for handling the request.
  -> Channel.OutChan (IdeRequest m)
     -- ^ Reading end of the channel where the requests are sent to this process.
  -> IdeM void
ideDispatcher env errorHandler callbackHandler pin =
  forever $ do
    debugm "ideDispatcher: top of loop"
    (IdeRequest tn d lid callback action) <- liftIO $ Channel.readChan pin
    liftIO $ traceEventIO $ "START " ++ show tn ++ "ide:" ++ d
    debugm
      $  "ideDispatcher: got request "
      ++ show tn
      ++ " with id: "
      ++ show lid

    iterT queueDeferred $ unlessCancelled env lid errorHandler $ do
      result <- action
      unlessCancelled env lid errorHandler $ liftIO $ do
        completedReq env lid
        case result of
          IdeResultOk x -> callbackHandler callback x
          IdeResultFail (IdeError _ msg _) ->
            errorHandler (Just lid) J.InternalError msg

    liftIO $ traceEventIO $ "STOP " ++ show tn ++ "ide:" ++ d
 where
  queueDeferred (Defer fp cacheCb) = lift $ modifyMTState $ \s ->
    let oldQueue = requestQueue s
        -- add to existing queue if possible
        update Nothing  = [cacheCb]
        update (Just x) = cacheCb : x
        newQueue = Map.alter (Just . update) fp oldQueue
    in  s { requestQueue = newQueue }

-- | Processes requests published in the channel and runs the give callback
-- or error handler as appropriate. Requests will not be processed if they
-- were cancelled before. If already in progress and then cancelled, the callback
-- will not be invoked in that case.
-- Meant to be run in a separate thread and be kept alive.
ghcDispatcher
  :: forall void m
   . DispatcherEnv
  -> ErrorHandler
  -> PublishDiagnostics
  -> CallbackHandler m
  -> Channel.OutChan (GhcRequest m)
  -> IdeGhcM void
ghcDispatcher env@DispatcherEnv { docVersionTVar } errorHandler publishDiagnostics callbackHandler pin
  = do
  iniDynFlags <- getSessionDynFlags
  forever $ do
    debugm "ghcDispatcher: top of loop"
    GhcRequest tn d context mver mid callback def action <- liftIO
      $ Channel.readChan pin
    debugm $ "ghcDispatcher:got request " ++ show tn ++ " with id: " ++ show mid
    liftIO $ traceEventIO $ "START " ++ show tn ++ "ghc:"  ++ d

    let
      runner :: a -> IdeGhcM a -> IdeGhcM (IdeResult  a)

      runner a act = case context of
        Nothing  -> runActionWithContext publishDiagnostics iniDynFlags Nothing a act
        Just uri -> case uriToFilePath uri of
          Just fp -> runActionWithContext publishDiagnostics iniDynFlags (Just fp) a act
          Nothing -> do
            debugm
              "ghcDispatcher:Got malformed uri, running action with default context"
            runActionWithContext publishDiagnostics iniDynFlags Nothing a act

    let
      runWithCallback = do
        result <- runner (pure def) action
        liftIO $ case join result of
          IdeResultOk   x                      -> callbackHandler callback x
          IdeResultFail err@(IdeError _ msg _) -> do
            logm $ "ghcDispatcher:Got error for a request: " ++ show err ++ " with mid: " ++ show mid
            errorHandler mid J.InternalError msg

    let
      runIfVersionMatch = case mver of
        Nothing            -> runWithCallback
        Just (uri, reqver) -> do
          curver <-
            liftIO
            $   STM.atomically
            $   Map.lookup uri
            <$> STM.readTVar docVersionTVar
          if Just reqver /= curver
            then debugm
              "ghcDispatcher:not processing request as it is for old version"
            else do
              debugm "ghcDispatcher:Processing request as version matches"
              runWithCallback

    case mid of
      Nothing  -> runIfVersionMatch
      Just lid -> unlessCancelled env lid errorHandler $ do
        liftIO $ completedReq env lid
        runIfVersionMatch
    liftIO $ traceEventIO $ "STOP " ++ show tn ++ "ghc:" ++ d

-- | Runs the passed monad only if the request identified by the passed LspId
-- has not already been cancelled.
unlessCancelled
  :: MonadIO m => DispatcherEnv -> J.LspId -> ErrorHandler -> m () -> m ()
unlessCancelled env lid errorHandler callback = do
  cancelled <- liftIO $ STM.atomically isCancelled
  if cancelled
    then liftIO $ do
      -- remove from cancelled and wip list
      STM.atomically $ STM.modifyTVar' (cancelReqsTVar env) (Set.delete lid)
      completedReq env lid
      errorHandler (Just lid) J.RequestCancelled ""
    else callback
  where isCancelled = Set.member lid <$> STM.readTVar (cancelReqsTVar env)

-- | Marks a request as completed by deleting the LspId from the
-- requestsInProgress Set.
completedReq :: DispatcherEnv -> J.LspId -> IO ()
completedReq env lid =
  STM.atomically $ STM.modifyTVar' (wipReqsTVar env) (Set.delete lid)
