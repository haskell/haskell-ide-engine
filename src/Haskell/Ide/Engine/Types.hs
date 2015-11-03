{-# LANGUAGE FlexibleInstances #-}
module Haskell.Ide.Engine.Types where

import           Control.Concurrent
import qualified Data.Map as Map
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Monad.Types as GM

type IdeM a = GM.GhcModT (GM.GmOutT IO) a

data Plugin = Plugin
    { initializeHook :: Maybe (IO ())
    }

defaultPlugin :: Plugin
defaultPlugin = Plugin
    { initializeHook = Nothing
    }

-- ---------------------------------------------------------------------

data PluginReg = PluginReg PluginDescriptor Dispatcher
type PluginId = String

type Plugins = Map.Map PluginId PluginReg


type RequestId = Int

data ChannelRequest = CReq
  { cinPlugin    :: PluginId
  , cinReqId     :: RequestId -- ^An identifier for the request, can tie back to
                              -- e.g. a promise id. It is returned with the
                              -- ChannelResponse.
  , cinReq       :: IdeRequest
  , cinReplyChan :: Chan ChannelResponse
  } deriving Show

instance Show (Chan ChannelResponse) where
  show _ = "(Chan ChannelResponse)"

data ChannelResponse = CResp
  { couPlugin :: PluginId
  , coutReqId :: RequestId
  , coutResp  :: IdeResponse
  } deriving Show
