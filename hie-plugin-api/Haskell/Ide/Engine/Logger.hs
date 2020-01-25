module Haskell.Ide.Engine.Logger where

import Control.Monad.IO.Class
import System.Log.Logger

logm :: MonadIO m => String -> m ()
logm s = liftIO $ infoM "hie" s

debugm :: MonadIO m => String -> m ()
debugm s = liftIO $ debugM "hie" s

warningm :: MonadIO m => String -> m ()
warningm s = liftIO $ warningM "hie" s

errorm :: MonadIO m => String -> m ()
errorm s = liftIO $ errorM "hie" s
