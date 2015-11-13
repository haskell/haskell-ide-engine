{-# LANGUAGE OverloadedStrings #-}
-- |Provide a protocol adapter/transport for JSON over stdio

module Haskell.Ide.Engine.Transport.JsonStdio where

import           Control.Concurrent
import           Control.Logging
import qualified Data.Aeson as A
import qualified Data.Map as Map
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
      debug $ T.pack $ "jsonStdioTransport:got:" ++ show req
      case req of
        Just (Left err) -> do
          putStr $ show $ "jsonStdioTransport:" ++ show err
          loop (cid + 1) stream'
        Just (Right r) -> do
          writeChan cin (wireToChannel cout cid r)
          rsp <- readChan cout
          BL.putStr $ A.encode (channelToWire rsp)
          loop (cid + 1) stream'
        Nothing -> do
          -- exit the loop
          putStr "jsonStdioTransport: Got Nothing"
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
                 { ideCommand = T.tail command
                 , ideParams  = params wr
                 }
    , cinReplyChan = cout
    }
    where
      (plugin,command) = T.break (==':') (cmd wr)

-- ---------------------------------------------------------------------

channelToWire :: ChannelResponse -> WireResponse
channelToWire cr = WireResp $ A.toJSON $ coutResp cr

-- ---------------------------------------------------------------------

data WireRequest = WireReq
  { cmd     :: T.Text -- ^combination of PluginId ":" CommandName
  , params  :: ParamMap
  } deriving (Show,Eq)

instance A.ToJSON WireRequest where
    toJSON wr = A.object
                [ "cmd" A..= cmd wr
                , "params" A..= params wr
                ]


instance A.FromJSON WireRequest where
    parseJSON (A.Object v) = WireReq <$>
                           v A..: "cmd" <*>
                           v A..:? "params" A..!= Map.empty
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

-- ---------------------------------------------------------------------

data WireResponse = WireResp A.Value
                  deriving (Show,Eq)

instance A.ToJSON WireResponse where
    toJSON (WireResp val) = val


instance A.FromJSON WireResponse where
    parseJSON p = return $ WireResp p
