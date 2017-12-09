{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | IdeM and associated types
module Haskell.Ide.Engine.MonadTypes
  (
  -- * Plugins
    PluginId
  , CommandName
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , IdePlugins(..)
  -- * The IDE monad
  , IdeM
  , IdeState(..)
  , AsyncM
  , liftAsync
  , MultiThreadState
  , readMTState
  , modifyMTState
  , runMTState
  , MonadMTState(..)
  -- * All the good types
  , module Haskell.Ide.Engine.PluginTypes
  ) where

import           Data.Aeson
import           Control.Monad.Reader
import Control.Concurrent.STM
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Dynamic
import           Haskell.Ide.Engine.PluginTypes
import qualified GhcMod.ModuleLoader as GM
import qualified GhcMod.Monad        as GM
import           GHC.Generics

type PluginId = T.Text
type CommandName = T.Text

data CommandFunc a b = CmdSync (a -> IdeM (IdeResponse b))
                     | CmdAsync ((IdeResponse b -> IO ()) -> a -> IdeM ())
                        -- ^ Asynchronous command that accepts a callback

data PluginCommand = forall a b. (FromJSON a, ToJSON b) =>
  PluginCommand { commandName :: CommandName
                , commandDesc :: T.Text
                , commandFunc :: CommandFunc a b
                }

data PluginDescriptor =
  PluginDescriptor { pluginName :: T.Text
                   , pluginDesc :: T.Text
                   , pluginCommands :: [PluginCommand]
                   } deriving (Show,Generic)

instance Show PluginCommand where
  show (PluginCommand name _ _) = "PluginCommand { name = " ++ T.unpack name ++ " }"

-- | a Description of the available commands stored in IdeM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId [PluginCommand]
  } deriving (Show,Generic)

instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ (fmap . fmap) (\x -> (commandName x, commandDesc x)) m

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
  writeMTS :: s -> m ()
  writeMTS s = modifyMTS (const s)

instance MonadMTState s (MultiThreadState s) where
  readMTS = readMTState
  modifyMTS = modifyMTState

-- ---------------------------------------------------------------------
type IdeM = GM.GhcModT AsyncM

instance MonadMTState IdeState IdeM where
  readMTS = lift $ lift $ readMTS
  modifyMTS f = lift $ lift $ modifyMTS f

type AsyncM = MultiThreadState IdeState

liftAsync :: AsyncM a -> IdeM a
liftAsync = lift . lift

data IdeState = IdeState
  { moduleCache :: GM.GhcModuleCache
  , idePlugins  :: IdePlugins
  , extensibleState :: !(Map.Map TypeRep Dynamic)
  } deriving (Show)

instance GM.HasGhcModuleCache AsyncM where
  getModuleCache = do
    tvar <- ask
    state <- liftIO $ readTVarIO tvar
    return (moduleCache state)
  setModuleCache mc = do
    tvar <- ask
    liftIO $ atomically $ modifyTVar' tvar (\st -> st { moduleCache = mc })

instance GM.HasGhcModuleCache IdeM where
  getModuleCache = lift . lift $ GM.getModuleCache
  setModuleCache = lift . lift . GM.setModuleCache

-- ---------------------------------------------------------------------
