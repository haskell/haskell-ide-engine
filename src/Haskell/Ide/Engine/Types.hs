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

data PluginRequest = forall a. ValidResponse a => PReq
  { pinReqId     :: RequestId -- ^An identifier for the request, can tie back to
                              -- e.g. a promise id. It is returned with the
                              -- ChannelResponse.
  , pinReq       :: IdeM a
  , pinReplyChan :: TChan PluginResponse
  }

data PluginResponse = PResp
  { poutReqId :: RequestId
  , poutResp  :: IdeResponse PluginResponseWrapper
  }

instance Show (TChan ChannelResponse) where
  show _ = "(TChan ChannelResponse)"

data ChannelResponse = CResp
  { couPlugin :: PluginId
  , coutReqId :: RequestId
  -- TODO: Pass the original type through, instead of Object. The other side of
  -- the dispatcher can sort out the ToJSON, if needed.
  , coutResp  :: IdeResponse Object
  } deriving (Show,Eq)
