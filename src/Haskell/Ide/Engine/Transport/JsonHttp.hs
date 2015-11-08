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

import           Control.Concurrent
import           Control.Monad.IO.Class
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
  -- curl -v http://localhost:8081/req/base -X POST -H Content-Type:application/json --data-binary '{"ideParams":{},"ideCommand":"version","ideContext":{"ctxEndPos":null,"ctxCabal":null,"ctxStartPos":null,"ctxFile":null}}'
       "req" :> Capture "plugin" Text
             :> QueryParam "rid" Int -- optional request id
             :> ReqBody '[JSON] IdeRequest
             :> Post '[JSON] IdeResponse

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
server :: Chan ChannelRequest ->  Chan ChannelResponse -> Server HieApi
server cin cout = hieH
              :<|> egH

  where
        hieH plugin mrid reqVal = do
          let rid = fromMaybe 1 mrid
          liftIO $ writeChan cin (CReq plugin rid reqVal cout)
          rsp <- liftIO $ readChan cout
          return (coutResp rsp)
          -- return (IdeResponseOk (String $ pack $ show r))

        egH = return (IdeRequest ("version"::Text) (Context Nothing Nothing Nothing Nothing) Map.empty)

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Chan ChannelRequest -> Chan ChannelResponse -> Application
test cin cout = serve testApi (server cin cout)

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Chan ChannelRequest -> Port -> IO ()
runTestServer cin port = do
  cout <- newChan :: IO (Chan ChannelResponse)
  run port (test cin cout)

-- Put this all to work!
jsonHttpListener :: Chan ChannelRequest -> Port -> IO ()
jsonHttpListener cin port = runTestServer cin port
