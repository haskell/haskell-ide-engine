{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
module Haskell.Ide.Engine.LSP.Reactor
  ( R
  , runReactor
  , reactorSend
  , reactorSend'
  , makeRequest
  , makeRequests
  , asksLspFuncs
  , REnv(..)
  )
where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.Messages as J
import qualified Language.Haskell.LSP.Types    as J
import           Haskell.Ide.Engine.Compat
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.LSP.Config
import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.Types

data REnv = REnv
  { dispatcherEnv     :: DispatcherEnv
  , reqChanIn         :: TChan (PluginRequest R)
  , lspFuncs          :: Core.LspFuncs Config
  , reactorPidCache   :: Int
  , diagnosticSources :: Map.Map DiagnosticTrigger [(PluginId,DiagnosticProviderFunc)]
  , hoverProviders    :: [HoverProvider]
  , symbolProviders   :: [SymbolProvider]
  -- TODO: Add code action providers here
  }

-- | The monad used in the reactor
type R = ReaderT REnv IO

instance HasPidCache R where
  getPidCache = asks reactorPidCache

-- ---------------------------------------------------------------------

runReactor
  :: Core.LspFuncs Config
  -> DispatcherEnv
  -> TChan (PluginRequest R)
  -> Map.Map DiagnosticTrigger [(PluginId,DiagnosticProviderFunc)]
  -> [HoverProvider]
  -> [SymbolProvider]
  -> R a
  -> IO a
runReactor lf de cin dps hps sps f = do
  pid <- getProcessID
  runReaderT f (REnv de cin lf pid dps hps sps)

-- ---------------------------------------------------------------------

asksLspFuncs :: MonadReader REnv m => (Core.LspFuncs Config -> a) -> m a
asksLspFuncs f = asks (f . lspFuncs)

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

reactorSend :: (MonadIO m, MonadReader REnv m) => J.FromServerMessage -> m ()
reactorSend msg = do
  sf <- asksLspFuncs Core.sendFunc
  liftIO $ sf msg

-- ---------------------------------------------------------------------

reactorSend'
  :: (MonadIO m, MonadReader REnv m) => (Core.SendFunc -> IO ()) -> m ()
reactorSend' f = do
  sf <- asksLspFuncs Core.sendFunc
  liftIO $ f sf

-- ---------------------------------------------------------------------

makeRequest :: (MonadIO m, MonadReader REnv m) => PluginRequest R -> m ()
makeRequest req@(GReq _ _ Nothing (Just lid) _ _) = writePluginReq req lid
makeRequest req@(IReq _ lid _ _) = writePluginReq req lid
makeRequest req = liftIO . atomically . flip writeTChan req =<< asks reqChanIn

writePluginReq :: (MonadIO m, MonadReader REnv m) => PluginRequest R -> J.LspId -> m ()
writePluginReq req lid = do
  wipTVar <- asks (wipReqsTVar . dispatcherEnv)
  cin     <- asks reqChanIn
  liftIO $ atomically $ do
    modifyTVar wipTVar (S.insert lid)
    writeTChan cin req

-- | Execute multiple ide requests sequentially
makeRequests :: [IdeM (IdeResponse a)] -- ^ The requests to make
             -> TrackingNumber
             -> J.LspId
             -> ([a] -> R ())          -- ^ Callback with the request inputs and results
             -> R ()
makeRequests = go []
  where
    go acc [] _ _ callback = callback acc
    go acc (x:xs) tn reqId callback =
      let reqCallback result = go (acc ++ [result]) xs tn reqId callback
      in makeRequest $ IReq tn reqId reqCallback x

-- ---------------------------------------------------------------------

