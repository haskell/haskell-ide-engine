{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | IdeGhcM and associated types
module Haskell.Ide.Engine.PluginsIdeMonads
  (
  -- * Plugins
    PluginId
  , CommandName
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , IdePlugins(..)
  -- * The IDE monad
  , IdeGhcM
  , IdeState(..)
  , IdeM
  , liftToGhc
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Dynamic (Dynamic)
import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Typeable (TypeRep, Typeable)

import qualified GhcMod.Monad        as GM
import           GHC.Generics
import           GHC (HscEnv)

import           Haskell.Ide.Engine.PluginTypes
import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.GhcModuleCache


type PluginId = T.Text
type CommandName = T.Text

newtype CommandFunc a b = CmdSync (a -> IdeGhcM (IdeResponse b))

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
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

-- | a Description of the available commands stored in IdeGhcM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId [PluginCommand]
  } deriving (Show,Generic)

instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ (fmap . fmap) (\x -> (commandName x, commandDesc x)) m

-- ---------------------------------------------------------------------

type IdeGhcM = GM.GhcModT IdeM

instance MonadMTState IdeState IdeGhcM where
  readMTS = lift $ lift $ readMTS
  modifyMTS f = lift $ lift $ modifyMTS f

type IdeM = MultiThreadState IdeState

liftToGhc :: IdeM a -> IdeGhcM a
liftToGhc = lift . lift

data IdeState = IdeState
  { moduleCache :: GhcModuleCache
  -- | A queue of actions to be performed once a module is loaded
  , actionQueue :: Map.Map FilePath [CachedModule -> IdeM ()]
  , idePlugins  :: IdePlugins
  , extensibleState :: !(Map.Map TypeRep Dynamic)
  , ghcSession  :: Maybe (IORef HscEnv)
  }

instance HasGhcModuleCache IdeM where
  getModuleCache = do
    tvar <- ask
    state <- liftIO $ readTVarIO tvar
    return (moduleCache state)
  setModuleCache mc = do
    tvar <- ask
    liftIO $ atomically $ modifyTVar' tvar (\st -> st { moduleCache = mc })

instance HasGhcModuleCache IdeGhcM where
  getModuleCache = lift . lift $ getModuleCache
  setModuleCache = lift . lift . setModuleCache
