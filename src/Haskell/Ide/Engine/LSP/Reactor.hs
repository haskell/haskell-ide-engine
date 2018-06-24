{-# LANGUAGE FlexibleContexts #-}
module Haskell.Ide.Engine.LSP.Reactor where

import Control.Monad.Reader
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Messages
import Haskell.Ide.Engine.LSP.Config

-- | The monad used in the reactor
type R = ReaderT (Core.LspFuncs Config) IO

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

reactorSend :: (MonadIO m, MonadReader (Core.LspFuncs Config) m) => FromServerMessage -> m ()
reactorSend msg = do
  sf <- asks Core.sendFunc
  liftIO $ sf msg

-- ---------------------------------------------------------------------

reactorSend' :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => (Core.SendFunc -> IO ()) -> m ()
reactorSend' f = do
  lf <- ask
  liftIO $ f (Core.sendFunc lf)

-- ---------------------------------------------------------------------
