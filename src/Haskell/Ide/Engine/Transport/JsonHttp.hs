{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Haskell.Ide.Engine.Transport.JsonHttp
  (
    jsonHttpListener
  )
  where

import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Lens
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger
import qualified Data.ByteString.Lazy.Char8 as BL8


-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type HieApi =
  -- curl -v http://localhost:8081/req/base -X POST -H Content-Type:application/json --data-binary '{"ideParams":{},"ideCommand":"version","ideContext":{"ctxEndPos":null,"ctxCabal":null,"ctxStartPos":null,"ctxFile":null}}'
       "req" :> Capture "plugin" PluginIdParam
             :> QueryParam "rid" RequestIdParam -- optional request id
             :> ReqBody '[JSON] IdeRequest
             :> Post '[JSON] (IdeResponse Object)

  :<|> "eg" :> Get '[JSON] IdeRequest
  -- GET /eg returns
  --  {"ideParams":{},"ideCommand":"version","ideContext":{"ctxEndPos":null,"ctxCabal":null,"ctxStartPos":null,"ctxFile":null}}

newtype PluginIdParam = PluginIdParam PluginId deriving (FromText)
newtype RequestIdParam = RequestIdParam {unRequestIdParam:: RequestId} deriving (FromText)

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
        hieH (PluginIdParam plugin) mrid reqVal = do
          let rid = maybe 1 unRequestIdParam mrid
          liftIO $ atomically $ writeTChan cin (CReq plugin rid reqVal cout)
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
jsonHttpListener cin port = do
  print "jsonHttpListener"
  BL8.writeFile "swagger.json" (encode swagDoc)
  runTestServer cin port


-- Swagger Doc
swagDoc :: SwaggerAPI
swagDoc = swagger (Proxy :: Proxy HieApi) mempty (BasePath "/") info schemes Nothing []
  where
    schemes = [ Http ]
    license' = APILicense "MIT" (Just "http://mit.com")
    info =
      Info
       (APITitle "Todo API") (APIVersion "1.0")
       (APIDescription "This is a an API that tests servant-swagger support for a Todo")
       (Just license')
       Nothing
       Nothing

instance ToSwaggerParamType PluginIdParam where
  toSwaggerParamType = const StringSwagParam

instance ToSwaggerDescription PluginIdParam where
  toSwaggerDescription = const "Plugin name"

instance ToSwaggerDescription RequestIdParam where
  toSwaggerDescription = const "Request Id"

instance ToSwaggerParamType RequestIdParam where
  toSwaggerParamType = const NumberSwagParam

instance ToSwaggerModel (IdeResponse resp) where
  toSwagModel Proxy =
    emptyModel
      & swagModelName .~ ModelName "IdeResponse"
      & swagProperties .~ []
      & swagDescription ?~ Description "A response from the IDE backend"
      & swagModelRequired .~ ["description"]

instance ToSwaggerModel IdeRequest where
 toSwagModel Proxy =
   emptyModel
     & swagModelName .~ ModelName "IdeRequest"
     & swagProperties .~ []
     & swagDescription ?~ Description "A request to the IDE backend"
     & swagModelRequired .~ ["description"]
