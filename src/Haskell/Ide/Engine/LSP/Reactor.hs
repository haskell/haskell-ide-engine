{-# LANGUAGE FlexibleContexts #-}
module Haskell.Ide.Engine.LSP.Reactor
  ( R
  , runReactor
  , reactorSend
  , reactorSend'
  , makeRequest
  , asksLspFuncs
  , REnv(..)
  )
where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.Messages as J
import qualified Language.Haskell.LSP.Types    as J
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.LSP.Config
import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.Types

data REnv = REnv
  { dispatcherEnv     :: DispatcherEnv
  , reqChanIn         :: TChan (PluginRequest R)
  , lspFuncs          :: Core.LspFuncs Config
  , diagnosticSources :: Map.Map DiagnosticTrigger [(PluginId,DiagnosticProviderFunc)]
  , commandPrefixer   :: T.Text -> T.Text
  }

-- | The monad used in the reactor
type R = ReaderT REnv IO

-- ---------------------------------------------------------------------

runReactor
  :: Core.LspFuncs Config
  -> DispatcherEnv
  -> TChan (PluginRequest R)
  -> Map.Map DiagnosticTrigger [(PluginId,DiagnosticProviderFunc)]
  -> (T.Text -> T.Text)
  -> R a
  -> IO a
runReactor lf de cin dps prefixer =
  flip runReaderT (REnv de cin lf dps prefixer)

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
