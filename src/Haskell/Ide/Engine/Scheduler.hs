{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE NamedFieldPuns            #-}
module Haskell.Ide.Engine.Scheduler
  ( Scheduler
  , DocUpdate
  , newScheduler
  , runScheduler
  , sendRequest
  , cancelRequest
  )
where

import           Control.Concurrent.Async       ( race_ )
import qualified Control.Concurrent.MVar       as MVar
import qualified Control.Concurrent.STM        as STM
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader.Class     ( ask )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad                  ( when )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified GhcMod.Types                  as GM
import qualified Language.Haskell.LSP.Types    as J
import qualified Language.Haskell.LSP.Types.Capabilities
                                               as C

import           Haskell.Ide.Engine.GhcModuleCache
import qualified Haskell.Ide.Engine.Compat     as Compat
import qualified Haskell.Ide.Engine.Channel    as Channel
import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.Engine.Dispatcher
import qualified Haskell.Ide.Engine.Monad      as M


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

 , ghcModOptions :: GM.Options
   -- ^ Options for the ghc-mod session. Since we only keep a single ghc-mod session
   -- at a time, this cannot be changed a runtime.

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


-- | Create a new scheduler parameterized with the monad of your choosing.
-- This is the monad where the handler for requests and handler for errors will run.
--
-- Once created, the scheduler needs to be run using 'runScheduler'
newScheduler
  :: IdePlugins
     -- ^ The list of plugins that will be used for responding to requests
  -> GM.Options
   -- ^ Options for the ghc-mod session. Since we only keep a single ghc-mod session
  -> IO (Scheduler m)
newScheduler plugins ghcModOptions = do
  cancelTVar  <- STM.atomically $ STM.newTVar Set.empty
  wipTVar     <- STM.atomically $ STM.newTVar Set.empty
  versionTVar <- STM.atomically $ STM.newTVar Map.empty
  ideChan     <- Channel.newChan
  ghcChan     <- Channel.newChan
  return $ Scheduler
    { plugins            = plugins
    , ghcModOptions      = ghcModOptions
    , requestsToCancel   = cancelTVar
    , requestsInProgress = wipTVar
    , documentVersions   = versionTVar
    , ideChan            = ideChan
    , ghcChan            = ghcChan
    }


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
  -> C.ClientCapabilities
     -- ^ List of features the IDE client supports or has enabled.
  -> IO ()
runScheduler Scheduler {..} errorHandler callbackHandler caps = do
  let dEnv = DispatcherEnv
        { cancelReqsTVar = requestsToCancel
        , wipReqsTVar    = requestsInProgress
        , docVersionTVar = documentVersions
        }

  stateVarVar <- MVar.newEmptyMVar
  pid         <- Compat.getProcessID


  let (_, ghcChanOut) = ghcChan
      (_, ideChanOut) = ideChan

  let initialState =
        IdeState emptyModuleCache Map.empty plugins Map.empty Nothing pid

      runGhcDisp = M.runIdeGhcM ghcModOptions caps initialState $ do
        stateVar <- lift . lift . lift $ ask
        liftIO $ MVar.putMVar stateVarVar stateVar
        ghcDispatcher dEnv errorHandler callbackHandler ghcChanOut

      runIdeDisp = do
        stateVar <- MVar.readMVar stateVarVar
        ideDispatcher stateVar caps dEnv errorHandler callbackHandler ideChanOut


  runGhcDisp `race_` runIdeDisp


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
  -> Maybe DocUpdate
    -- ^ If not Nothing, the version for the given document is updated before dispatching.
  -> PluginRequest m
    -- ^ The request to dispatch.
  -> IO ()
sendRequest Scheduler {..} docUpdate req = do
  let (ghcChanIn, _) = ghcChan
      (ideChanIn, _) = ideChan

  case docUpdate of
    Nothing -> pure ()
    Just (uri, ver) ->
      STM.atomically $ STM.modifyTVar' documentVersions (Map.insert uri ver)

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
