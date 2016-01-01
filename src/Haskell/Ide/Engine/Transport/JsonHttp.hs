{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Haskell.Ide.Engine.Transport.JsonHttp
  (
    jsonHttpListener
  , PluginType(..)
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
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Internal

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

data PluginType = PluginType Symbol [Symbol]

type PluginRoute (s::Symbol) r = "req" :> s :> r

type CommandRoute (s :: Symbol) =
     s :>
     QueryParam "rid" Int :>
     ReqBody '[JSON] ParamMap :>
     Post '[JSON] (IdeResponse Object)

type family PluginRoutes list where
  PluginRoutes ('PluginType name cmds ': xs)
     = (PluginRoute name (CommandRoutes cmds)) :<|> PluginRoutes xs
  PluginRoutes '[] = "eg" :> Get '[JSON] IdeRequest

type family CommandRoutes list where
  CommandRoutes '[] = Fail
  CommandRoutes (cmd ': cmds) = CommandRoute cmd :<|> CommandRoutes cmds

data Fail = Fail

instance HasServer Fail where

  type ServerT Fail m = Fail

  route _ _ _ f = f (failWith NotFound)

testApi :: Proxy plugins -> Proxy (PluginRoutes plugins)
testApi _ = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.

class HieServer (list :: [PluginType]) where
  hieServer :: Proxy list
            -> TChan ChannelRequest
            -> TChan ChannelResponse
            -> Server (PluginRoutes list)

instance HieServer '[] where
  hieServer _ _ _ = return (IdeRequest ("version"::Text) Map.empty)

instance (KnownSymbol plugin,CommandServer cmds,HieServer xs) => HieServer ('PluginType plugin cmds ': xs) where
  hieServer _ cin cout =
    pluginHandler :<|> hieServer (Proxy :: Proxy xs) cin cout
    where pluginHandler
            :: Server (PluginRoute plugin (CommandRoutes cmds))
          pluginHandler =
            cmdServer (Proxy :: Proxy plugin)
                      (Proxy :: Proxy cmds)
                      cin
                      cout

class CommandServer (list :: [Symbol]) where
  cmdServer :: KnownSymbol plugin
            => Proxy plugin
            -> Proxy list
            -> TChan ChannelRequest
            -> TChan ChannelResponse
            -> Server (CommandRoutes list)

instance CommandServer '[] where
  cmdServer _ _ _ _ = Fail

instance (KnownSymbol x,CommandServer xs) => CommandServer (x ': xs) where
  cmdServer plugin _ cin cout =
    pluginHandler :<|> (cmdServer plugin (Proxy :: Proxy xs) cin cout)
    where pluginHandler
            :: Server (CommandRoute x)
          pluginHandler mrid reqVal =
            do let rid = fromMaybe 1 mrid
               liftIO $
                 atomically $
                 writeTChan
                   cin
                   (CReq (T.pack $ symbolVal plugin)
                         rid
                         (IdeRequest (T.pack $ symbolVal (Proxy :: Proxy x))
                                     reqVal)
                         cout)
               rsp <- liftIO $ atomically $ readTChan cout
               return (coutResp rsp)

server :: HieServer plugins => Proxy plugins -> TChan ChannelRequest ->  TChan ChannelResponse -> Server (PluginRoutes plugins)
server proxy cin cout = hieServer proxy cin cout

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: (HieServer plugins, HasServer (PluginRoutes plugins)) => Proxy plugins -> TChan ChannelRequest -> TChan ChannelResponse -> Application
test proxy cin cout = serve (testApi proxy) (server proxy cin cout)

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: (HieServer plugins, HasServer (PluginRoutes plugins)) => Proxy plugins -> TChan ChannelRequest -> Port -> IO ()
runTestServer proxy cin port = do
  cout <- atomically newTChan :: IO (TChan ChannelResponse)
  run port (test proxy cin cout)

-- Put this all to work!
jsonHttpListener :: (HieServer plugins, HasServer (PluginRoutes plugins)) => Proxy plugins -> TChan ChannelRequest -> Port -> IO ()
jsonHttpListener proxy cin port = runTestServer proxy cin port
