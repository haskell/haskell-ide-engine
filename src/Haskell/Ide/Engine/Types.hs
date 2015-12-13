{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.Engine.Types where

import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Haskell.Ide.Engine.PluginDescriptor

-- ---------------------------------------------------------------------

type RequestId = Int

data ChannelRequest = CReq
  { cinPlugin    :: PluginId
  , cinReqId     :: RequestId -- ^An identifier for the request, can tie back to
                              -- e.g. a promise id. It is returned with the
                              -- ChannelResponse.
  , cinReq       :: IdeRequest
  , cinReplyChan :: TChan ChannelResponse
  } deriving Show

instance Show (TChan ChannelResponse) where
  show _ = "(TChan ChannelResponse)"

data ChannelResponse = CResp
  { couPlugin :: PluginId
  , coutReqId :: RequestId
  , coutResp  :: IdeResponse Object
  } deriving (Show,Eq)
