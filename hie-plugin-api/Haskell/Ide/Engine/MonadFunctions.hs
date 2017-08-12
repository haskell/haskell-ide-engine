{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- For MonadLogger IO instance
module Haskell.Ide.Engine.MonadFunctions
  (
  -- * Logging functions
    logm
  , debugm
  ) where

import Control.Monad.IO.Class
import System.Log.Logger

-- ---------------------------------------------------------------------

logm :: MonadIO m => String -> m ()
logm s = liftIO $ infoM "hie" s

debugm :: MonadIO m => String -> m ()
debugm s = liftIO $ debugM "hie" s

-- ---------------------------------------------------------------------
