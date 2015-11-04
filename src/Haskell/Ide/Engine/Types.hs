{-# LANGUAGE FlexibleInstances #-}

module Haskell.Ide.Engine.Types where

import           Control.Concurrent
import           Haskell.Ide.Engine.PluginDescriptor

-- ---------------------------------------------------------------------


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
