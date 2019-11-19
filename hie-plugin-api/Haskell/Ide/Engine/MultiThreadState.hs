{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Haskell.Ide.Engine.MultiThreadState
  ( MultiThreadState
  , readMTState
  , modifyMTState
  , runMTState
  , MonadMTState(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader

-- ---------------------------------------------------------------------

type MultiThreadState s = ReaderT (TVar s) IO

readMTState :: MultiThreadState s s
readMTState = ask >>= liftIO . readTVarIO

modifyMTState :: (s -> s) -> MultiThreadState s ()
modifyMTState f = do
  tvar <- ask
  liftIO $ atomically $ modifyTVar' tvar f

runMTState :: MultiThreadState s a -> s -> IO a
runMTState m s = do
  tv <- newTVarIO s
  runReaderT m tv

class MonadIO m => MonadMTState s m | m -> s where
  readMTS :: m s
  modifyMTS :: (s -> s) -> m ()

instance MonadMTState s (MultiThreadState s) where
  readMTS = readMTState
  modifyMTS = modifyMTState
