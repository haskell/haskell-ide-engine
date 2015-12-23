{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Haskell.Ide.Engine.Transport.JsonHttp
  (
    jsonHttpListener
  )
  where

import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type HieApi =
       "req" :> Capture "plugin" Text
             :> Capture "command" CommandName
             :> QueryParam "rid" Int -- optional request id
             :> ReqBody '[JSON] ParamMap
             :> Post '[JSON] (IdeResponse Object)

  :<|> "eg" :> Get '[JSON] IdeRequest
  -- GET /eg returns
  --  {"ideParams":{},"ideCommand":"version","ideContext":{"ctxEndPos":null,"ctxCabal":null,"ctxStartPos":null,"ctxFile":null}}


testApi :: Proxy HieApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.
server :: TChan ChannelRequest ->  TChan ChannelResponse -> Server HieApi
server cin cout = hieH
              :<|> egH

  where
        hieH plugin command mrid reqVal = do
          let rid = fromMaybe 1 mrid
          liftIO $ atomically $ writeTChan cin (CReq plugin rid (IdeRequest command reqVal) cout)
          rsp <- liftIO $ atomically $ readTChan cout
          return (coutResp rsp)
          -- return (IdeResponseOk (String $ pack $ show r))

        egH = return (IdeRequest ("version"::Text) Map.empty)

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: TChan ChannelRequest -> TChan ChannelResponse -> Application
test cin cout = serve testApi (server cin cout)

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: TChan ChannelRequest -> Port -> IO ()
runTestServer cin port = do
  cout <- atomically newTChan :: IO (TChan ChannelResponse)
  run port (test cin cout)

-- Put this all to work!
jsonHttpListener :: TChan ChannelRequest -> Port -> IO ()
jsonHttpListener cin port = runTestServer cin port
