{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
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
  , ExtensionClass(..)
  , UriCache(..)
  , ModuleCache(..)
  , CachedModule(..)
  , LocMap
  -- * All the good types
  , module Haskell.Ide.Engine.PluginTypes
  ) where

import           Data.Aeson
import           Control.Monad.State.Strict
import           Data.Dynamic
import qualified Data.Map as Map
import qualified Data.Text as T
import           Haskell.Ide.Engine.PluginTypes
import qualified GhcMod.Monad as GM
import qualified GhcMod.Types as GM
import           GHC(TypecheckedModule)
import           GHC.Generics

import qualified Data.IntervalMap.FingerTree as IM
import qualified Name as GHC

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
  {
    idePlugins :: IdePlugins
  , extensibleState :: !(Map.Map TypeRep Dynamic)
              -- ^ stores custom state information.
  , cradleCache :: !(Map.Map FilePath GM.Cradle)
              -- ^ map from dirs to cradles
  , uriCaches  :: !UriCaches
  } deriving (Show)

type UriCaches = Map.Map Uri UriCache

data UriCache = UriCache
  { cachedModule :: !CachedModule
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  } deriving Show

type LocMap = IM.IntervalMap Position GHC.Name

data CachedModule = CachedModule
  { tcMod       :: !TypecheckedModule
  , locMap      :: !LocMap
  , revMap      :: !(FilePath -> FilePath)
  , newPosToOld :: !(Position -> Maybe Position)
  , oldPosToNew :: !(Position -> Maybe Position)
  }

instance Show CachedModule where
  show CachedModule{} = "CachedModule { .. }"

-- ---------------------------------------------------------------------
-- Extensible state, based on
-- http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html#t:ExtensionClass
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    -- | Defines an initial value for the state extension
    initialValue :: a

-- | A ModuleCache is valid for the lifetime of a CachedModule
-- It is generated on need and the cache is invalidated
-- when a new CachedModule is loaded.
-- Allows the caching of arbitary data linked to a particular
-- TypecheckedModule.
class Typeable a => ModuleCache a where
    -- | Defines an initial value for the state extension
    cacheDataProducer :: CachedModule -> IdeM a

instance ModuleCache () where
    cacheDataProducer = const $ return ()
