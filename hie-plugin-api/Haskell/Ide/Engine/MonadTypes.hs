{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
  -- * All the good types
  , module Haskell.Ide.Engine.PluginTypes
  ) where

import           Data.Aeson
import           Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.Text as T
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

type IdeM = IdeT IO
type IdeT m = GM.GhcModT (StateT IdeState m)

data IdeState = IdeState
  { idePlugins  :: IdePlugins
  , moduleCache :: GM.GhcModuleCache
  } deriving (Show)

instance (Monad m) => GM.HasGhcModuleCache (IdeT m) where
  getModuleCache    = lift . lift $ gets moduleCache
  setModuleCache mc = lift . lift $ modify (\s -> s { moduleCache = mc })

-- ---------------------------------------------------------------------
