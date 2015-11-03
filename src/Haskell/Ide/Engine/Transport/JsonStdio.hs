{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Provide a protocol adapter/transport for JSON over stdio

module Haskell.Ide.Engine.Transport.JsonStdio where

import           Control.Concurrent
import           Control.Logging
import           Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Char
import           Data.List
import qualified Data.Map as Map
import           GHC.Generics
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Plugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Pipes
import qualified Pipes.Aeson as P
import qualified Pipes.ByteString as P
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Pipes.Parse
import           Pipes.Prelude hiding (show)
import           System.IO

-- TODO: Can pass in a handle, then it is general
jsonStdioTransport :: Chan ChannelRequest -> IO ()
jsonStdioTransport cin = do
  cout <- newChan :: IO (Chan ChannelResponse)
  hSetBuffering stdout NoBuffering
  let
    loop cid stream = do
      debug "jsonStdioTransport:calling go"
      (req,stream') <- runStateT decodeMsg stream
      case req of
        Just (Left err) -> putStr $ show (HieError (A.String $ T.pack $ show err))
        Just (Right r) -> do
          writeChan cin (wireToChannel cout cid r)
          rsp <- readChan cout
          BL.putStr $ A.encode (channelToWire rsp)
        Nothing -> putStr $ show (HieError (A.String $ T.pack $ "Got Nothing"))
      debug $ T.pack $ "jsonStdioTransport:got:" ++ show req
      loop (cid + 1) stream'
  loop 1 P.stdin

decodeMsg :: (Monad m) => Parser B.ByteString m (Maybe (Either P.DecodingError WireRequest))
decodeMsg = P.decode

-- to help with type inference
printTest :: (MonadIO m) => Consumer' [Int] m r
printTest = P.print

-- ---------------------------------------------------------------------

wireToChannel :: Chan ChannelResponse -> RequestId -> WireRequest -> ChannelRequest
wireToChannel cout ri wr =
  CReq
    { cinPlugin = plugin
    , cinReqId = ri
    , cinReq = IdeRequest
                 { ideCommand = tail command
                 , ideSession = session wr
                 , ideContext = context wr
                 , ideParams  = params wr
                 }
    , cinReplyChan = cout
    }
    where
      (plugin,command) = break (==':') (T.unpack $ cmd wr)

-- ---------------------------------------------------------------------

channelToWire :: ChannelResponse -> WireResponse
channelToWire cr =
  case coutResp cr of
    IdeResponseOk v -> Ok v
    IdeResponseFail v -> Fail v
    IdeResponseError v -> HieError v

-- ---------------------------------------------------------------------

data WireRequest = WireReq
  { cmd     :: T.Text -- ^combination of PluginId ":" CommandName
  , session :: SessionContext
  , context :: CommandContext
  , params  :: Map.Map ParamId ParamVal
  } deriving (Show,Eq,Generic)

instance A.ToJSON WireRequest where
    toJSON = A.genericToJSON A.defaultOptions


instance A.FromJSON WireRequest
    -- No need to provide a parseJSON implementation.

-- ---------------------------------------------------------------------

data WireResponse = Ok A.Value | Fail A.Value | HieError A.Value
                  deriving (Show,Eq,Generic)

instance A.ToJSON WireResponse where
    toJSON = A.genericToJSON A.defaultOptions


instance A.FromJSON WireResponse
    -- No need to provide a parseJSON implementation.
