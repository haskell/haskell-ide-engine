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
  , IdeResponse(..)
  , IdeError(..)
  , IdeErrorCode(..)
  , CommandName
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , IdePlugins(..)
  -- * The IDE monad
  , IdeGhcM
  , IdeState(..)
  , IdeM
  , LiftsToIdeGhcM
  , liftIdeGhcM
  ) where

import           Control.Concurrent.STM
import           Control.Monad
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

import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.GhcModuleCache


type PluginId = T.Text
type CommandName = T.Text

-- | The IDE response, with the type of response it contains
data IdeResponse a = IdeResponseOk a
                   | IdeResponseDeferred FilePath (CachedModule -> IdeGhcM (IdeResponse a))
                   | IdeResponseFail IdeError

instance Functor IdeResponse where
  fmap f (IdeResponseOk x) = IdeResponseOk (f x)
  fmap f (IdeResponseDeferred fp cb) = IdeResponseDeferred fp $ cb >=> (return . fmap f)
  fmap _ (IdeResponseFail err) = IdeResponseFail err

-- | Error codes. Add as required
data IdeErrorCode
  = ParameterError          -- ^ Wrong parameter type
  | PluginError             -- ^ An error returned by a plugin
  | InternalError           -- ^ Code error (case not handled or deemed
                            --   impossible)
  | UnknownPlugin           -- ^ Plugin is not registered
  | UnknownCommand          -- ^ Command is not registered
  | InvalidContext          -- ^ Context invalid for command
  | OtherError              -- ^ An error for which there's no better code
  | ParseError              -- ^ Input could not be parsed
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic)
 
instance ToJSON IdeErrorCode
instance FromJSON IdeErrorCode
 
-- | A more structured error than just a string
data IdeError = IdeError
  { ideCode    :: IdeErrorCode -- ^ The error code
  , ideMessage :: T.Text       -- ^ A human readable message
  , ideInfo    :: Value        -- ^ Additional information
  }
  deriving (Show,Read,Eq,Generic)

instance ToJSON IdeError
instance FromJSON IdeError

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

class LiftsToIdeGhcM m where
  liftIdeGhcM :: m a -> IdeGhcM a

instance LiftsToIdeGhcM IdeM where
  liftIdeGhcM = liftIdeGhcM

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
