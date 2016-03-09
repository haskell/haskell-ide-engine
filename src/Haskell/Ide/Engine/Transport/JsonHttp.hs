{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskell.Ide.Engine.Transport.JsonHttp
  ( jsonHttpListener
  , PluginType(..)
  , Plugin(..)
  ) where

import           Control.Applicative
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.Types hiding (parse)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Singletons.Prelude hiding ((:>))
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.JsonHttp.Undecidable
import           Haskell.Ide.Engine.Types
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.Server.Internal

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

-- | A greet message data type
newtype Greet = Greet { _msg :: T.Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet where
  toJSON = genericToJSON defaultOptions

newtype TaggedMap (tags :: [ParamDescType]) = TaggedMap ParamMap deriving (Monoid)

instance TaggedMapParser tags => FromJSON (TaggedMap tags) where
  parseJSON (Object v) = fmap (TaggedMap . Map.fromList) (parseTaggedMap (Proxy :: Proxy tags) v)
  parseJSON _ =  empty

class TaggedMapParser (tags :: [ParamDescType]) where
  parseTaggedMap :: Proxy tags -> Object -> Parser [(ParamId,ParamValP)]

instance TaggedMapParser '[] where
  parseTaggedMap _ _ = pure []

instance (ParamParser x, TaggedMapParser xs) => TaggedMapParser (x ': xs) where
  parseTaggedMap _ v = liftA2 (:) (parseParam (Proxy :: Proxy x) v) $ parseTaggedMap (Proxy :: Proxy xs) v

class ParamParser (t :: ParamDescType) where
  parseParam :: Proxy t -> Object -> Parser (ParamId,ParamValP)

instance (KnownSymbol pname, FromJSON (ParamVal ptype)) => ParamParser ('ParamDescType pname phelp ptype preq) where
  parseParam _ v =
    do unparsedParamVal <- v .: paramId
       paramVal <- parseJSON unparsedParamVal :: Parser (ParamVal ptype)
       pure (paramId,ParamValP paramVal)
    where paramId = T.pack $ symbolVal (Proxy :: Proxy pname)

data Plugin (t :: PluginType) where
  Plugin :: KnownSymbol name => Proxy name -> TaggedPluginDescriptor cmds -> Plugin ('PluginType name cmds)

data PluginType = PluginType Symbol [CommandType]

-- | Build up a list of all plugins and their commands, as a servant API spec
type family PluginRoutes (list :: [PluginType]) where
  PluginRoutes ('PluginType name cmds ': xs)
     = PluginRoute name (CommandRoutes cmds) :<|> PluginRoutes xs
  PluginRoutes '[] = "eg" :> Get '[JSON] IdeRequest

-- | All plugin commands are prefixed by "req" and the plugin name.
type PluginRoute (s::Symbol) r = "req" :> s :> r


-- | Build up a list of all commands, as a servant API spec
type family CommandRoutes (list :: [CommandType]) where
  CommandRoutes '[] = Fail
  CommandRoutes ('CommandType name cxts params ': cmds)
     = CommandRoute name (CommandParams cxts params) :<|> CommandRoutes cmds

-- | Within a plugin route, the command is routed according to its name, and can
-- take an optional request id query parameter. The rest of the parameters are
-- passed in the body, JSON-encoded.
type CommandRoute (name :: Symbol) (params :: [ParamDescType]) =
   name :>
   QueryParam "rid" Int :>
   ReqBody '[JSON] (TaggedMap params) :>
   Post '[JSON] (IdeResponse Object)


data Fail = Fail

instance HasServer Fail where

  type ServerT Fail m = Fail

  route _ _ _ f = f (failWith NotFound)

hieAPI :: Proxy plugins -> Proxy (PluginRoutes plugins)
hieAPI _ = Proxy

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
  hieServer _ _ _ = return (IdeRequest ("version"::T.Text) Map.empty)

instance (KnownSymbol plugin,CommandServer cmds,HieServer xs)
  => HieServer ('PluginType plugin cmds ': xs) where
  hieServer _ cin cout =
    pluginHandler :<|> hieServer (Proxy :: Proxy xs) cin cout
    where pluginHandler
            :: Server (PluginRoute plugin (CommandRoutes cmds))
          pluginHandler =
            cmdServer (Proxy :: Proxy plugin)
                      (Proxy :: Proxy cmds)
                      cin
                      cout

class CommandServer (list :: [CommandType]) where
  cmdServer :: KnownSymbol plugin
            => Proxy plugin
            -> Proxy list
            -> TChan ChannelRequest
            -> TChan ChannelResponse
            -> Server (CommandRoutes list)

instance CommandServer '[] where
  cmdServer _ _ _ _ = Fail

instance (KnownSymbol x,CommandServer xs)
  => CommandServer ('CommandType x cxts params ': xs) where
  cmdServer plugin _ cin cout =
    cmdHandler plugin (Proxy :: Proxy x) cin cout :<|> cmdServer plugin (Proxy :: Proxy xs) cin cout

cmdHandler :: (KnownSymbol plugin,KnownSymbol cmd)
           => Proxy plugin
           -> Proxy cmd
           -> TChan ChannelRequest
           -> TChan ChannelResponse
           -> Server (CommandRoute x params)
cmdHandler plugin cmd cin cout mrid (TaggedMap reqVal) =
            do let rid = fromMaybe 1 mrid
               liftIO $
                 atomically $
                 writeTChan
                   cin
                   (CReq (T.pack $ symbolVal plugin)
                         rid
                         (IdeRequest (T.pack $ symbolVal cmd)
                                     reqVal)
                         cout)
               rsp <- liftIO $ atomically $ readTChan cout
               return (coutResp rsp)

hieServantServer :: HieServer plugins
       => Proxy plugins -> TChan ChannelRequest ->  TChan ChannelResponse -> Servant.Server (PluginRoutes plugins)
hieServantServer proxy cin cout = hieServer proxy cin cout

waiApp :: (HieServer plugins, HasServer (PluginRoutes plugins))
     => Proxy plugins -> TChan ChannelRequest -> TChan ChannelResponse -> Application
waiApp proxy cin cout = Servant.serve (hieAPI proxy) (hieServantServer proxy cin cout)

runHttpServer :: (HieServer plugins, HasServer (PluginRoutes plugins)) => Proxy plugins -> TChan ChannelRequest -> Port -> IO ()
runHttpServer proxy cin port = do
  cout <- atomically newTChan :: IO (TChan ChannelResponse)
  Warp.run port (waiApp proxy cin cout)

-- Put this all to work!
jsonHttpListener :: (HieServer plugins, HasServer (PluginRoutes plugins))
                 => Proxy plugins -> TChan ChannelRequest -> Port -> IO ()
jsonHttpListener proxy cin port = runHttpServer proxy cin port
