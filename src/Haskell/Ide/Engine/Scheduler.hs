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


data Scheduler m = Scheduler
 { plugins :: IdePlugins
 , ghcModOptions :: GM.Options
 , requestsToCancel :: STM.TVar (Set.Set J.LspId)
 , requestsInProgress :: STM.TVar (Set.Set J.LspId)
 , documentVersions :: STM.TVar (Map.Map Uri Int)
 , ideChan :: (Channel.InChan (IdeRequest m), Channel.OutChan (IdeRequest m))
 , ghcChan :: (Channel.InChan (GhcRequest m), Channel.OutChan (GhcRequest m))
 }

type DocUpdate = (Uri, Int)


newScheduler :: IdePlugins -> GM.Options -> IO (Scheduler m)
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


runScheduler
  :: forall m
   . Scheduler m
  -> ErrorHandler
  -> CallbackHandler m
  -> C.ClientCapabilities
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


sendRequest
  :: forall m . Scheduler m -> Maybe DocUpdate -> PluginRequest m -> IO ()
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

cancelRequest :: forall m . Scheduler m -> J.LspId -> IO ()
cancelRequest Scheduler { requestsToCancel, requestsInProgress } lid =
  STM.atomically $ do
    wip <- STM.readTVar requestsInProgress
    when (Set.member lid wip)
      $ STM.modifyTVar' requestsToCancel (Set.insert lid)
