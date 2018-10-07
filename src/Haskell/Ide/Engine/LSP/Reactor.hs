{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
module Haskell.Ide.Engine.LSP.Reactor
  ( R
  , runReactor
  , reactorSend
  , reactorSend'
  , makeRequest
  , makeRequests
  , updateDocumentRequest
  , cancelRequest
  , asksLspFuncs
  , REnv(..)
  )
where

import           Control.Monad.Reader
import qualified Data.Map                      as Map
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.Messages as J
import qualified Language.Haskell.LSP.Types    as J
import           Haskell.Ide.Engine.Compat
import           Haskell.Ide.Engine.LSP.Config
import           Haskell.Ide.Engine.PluginsIdeMonads
import qualified Haskell.Ide.Engine.Scheduler  as Scheduler
import           Haskell.Ide.Engine.Types

data REnv = REnv
  { scheduler         :: Scheduler.Scheduler R
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
  -> Scheduler.Scheduler R
  -> Map.Map DiagnosticTrigger [(PluginId, DiagnosticProviderFunc)]
  -> [HoverProvider]
  -> [SymbolProvider]
  -> R a
  -> IO a
runReactor lf sc dps hps sps f = do
  pid <- getProcessID
  runReaderT f (REnv sc lf pid dps hps sps)

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
makeRequest req = do
  sc <- asks scheduler
  liftIO $ Scheduler.sendRequest sc Nothing req

updateDocumentRequest
  :: (MonadIO m, MonadReader REnv m) => Uri -> Int -> PluginRequest R -> m ()
updateDocumentRequest uri ver req = do
  sc <- asks scheduler
  liftIO $ Scheduler.sendRequest sc (Just (uri, ver)) req

cancelRequest :: (MonadIO m, MonadReader REnv m) => J.LspId -> m ()
cancelRequest lid =
  liftIO . flip Scheduler.cancelRequest lid =<< asks scheduler

-- | Execute multiple ide requests sequentially
makeRequests
  :: [IdeDeferM (IdeResult a)] -- ^ The requests to make
  -> TrackingNumber
  -> J.LspId
  -> ([a] -> R ())          -- ^ Callback with the request inputs and results
  -> R ()
makeRequests = go []
 where
  go acc [] _ _ callback = callback acc
  go acc (x : xs) tn reqId callback =
    let reqCallback result = go (acc ++ [result]) xs tn reqId callback
    in  makeRequest $ IReq tn reqId reqCallback x

-- ---------------------------------------------------------------------
