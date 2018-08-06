{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Haskell.Ide.Engine.MultiThreadState
  ( MultiThreadState(..)
  , runMTState
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import qualified GhcMod.Monad as GM
import Control.Monad.Trans.Control
import Control.Monad.Base
import Exception

-- ---------------------------------------------------------------------

newtype MultiThreadState s a = MTState { getMTState :: ReaderT (TVar s) IO a }
  deriving (Functor, Applicative, Monad, GM.MonadIO, MonadIO, MonadReader (TVar s), MonadBase IO, ExceptionMonad)

instance MonadBaseControl IO (MultiThreadState s) where
  type StM (MultiThreadState s) a = a 
  liftBaseWith f = MTState $ liftBaseWith $ \q -> f (q . getMTState)
  restoreM = MTState . restoreM 

runMTState :: MultiThreadState s a -> s -> IO a
runMTState m s = do
  tv <- newTVarIO s
  runReaderT (getMTState m) tv

instance MonadState s (MultiThreadState s) where
  state f = do
    tvar <- ask
    liftIO $ atomically $ do
      s <- readTVar tvar
      let (a, s') = f s
      writeTVar tvar s'
      return a
