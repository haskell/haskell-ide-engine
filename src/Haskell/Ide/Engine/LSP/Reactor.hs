{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
  , getClientConfig
  , REnv(..)
  )
where

import           Control.Monad.Reader
import qualified Data.Map                      as Map
import qualified Data.Default
import           Data.Maybe                     ( fromMaybe )
import           Haskell.Ide.Engine.Compat
import           Haskell.Ide.Engine.Config
import           Haskell.Ide.Engine.PluginsIdeMonads
import qualified Haskell.Ide.Engine.Scheduler  as Scheduler
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.Messages as J
import qualified Language.Haskell.LSP.Types    as J

-- ---------------------------------------------------------------------

data REnv = REnv
  { scheduler           :: Scheduler.Scheduler R
  , lspFuncs            :: Core.LspFuncs Config
  -- | The process ID of HIE. See 'HasPidCache'
  , reactorPidCache     :: Int
  , diagnosticSources   :: Map.Map DiagnosticTrigger [(PluginId,DiagnosticProviderFunc)]
  , hoverProviders      :: [HoverProvider]
  , symbolProviders     :: [SymbolProvider]
  , formattingProviders :: Map.Map PluginId FormattingProvider
  -- | Ide Plugins that are available
  , idePlugins          :: IdePlugins
  -- TODO: Add code action providers here
  }

-- | The monad used in the reactor
type R = ReaderT REnv IO

instance HasPidCache R where
  getPidCache = asks reactorPidCache

instance Scheduler.HasScheduler REnv R where
  getScheduler = scheduler

-- ---------------------------------------------------------------------

runReactor
  :: Core.LspFuncs Config
  -> Scheduler.Scheduler R
  -> Map.Map DiagnosticTrigger [(PluginId, DiagnosticProviderFunc)]
  -> [HoverProvider]
  -> [SymbolProvider]
  -> Map.Map PluginId FormattingProvider
  -> IdePlugins
  -> R a
  -> IO a
runReactor lf sc dps hps sps fps plugins f = do
  pid <- getProcessID
  runReaderT f (REnv sc lf pid dps hps sps fps plugins)

-- ---------------------------------------------------------------------

asksLspFuncs :: MonadReader REnv m => (Core.LspFuncs Config -> a) -> m a
asksLspFuncs f = asks (f . lspFuncs)

-- | Returns the current client configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can at runitime change
-- their configuration.
--
-- If no custom configuration has been set by the client, this function returns
-- our own defaults.
getClientConfig :: (MonadIO m, MonadReader REnv m) => m Config
getClientConfig = do
  lf <- asks lspFuncs
  liftIO $ fromMaybe Data.Default.def <$> Core.config lf

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

-- | Sends a single request to the scheduler so it can be be processed
-- asynchronously.
makeRequest :: (MonadIO m, MonadReader REnv m) => PluginRequest R -> m ()
makeRequest = Scheduler.makeRequest

-- | Updates the version of a document and then sends the request to be processed
-- asynchronously.
updateDocumentRequest
  :: (MonadIO m, MonadReader REnv m) => Uri -> Int -> PluginRequest R -> m ()
updateDocumentRequest = Scheduler.updateDocumentRequest

-- | Marks a s requests as cencelled by its LspId
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
