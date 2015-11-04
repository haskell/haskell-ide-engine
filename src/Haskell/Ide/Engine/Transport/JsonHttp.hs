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
-- import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()

  :<|> "req" :> Capture "plugin" String :> Capture "cmd" String :> QueryParam "capital" Bool :> Get '[JSON] IdeResponse

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.
server :: Chan ChannelRequest ->  Chan ChannelResponse -> Server TestApi
server cin cout = helloH :<|> postGreetH :<|> deleteGreetH
              :<|> hieH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True)  = return . Greet . toUpper $ "Hello, " <> name

        postGreetH greet = return greet

        deleteGreetH _ = return ()

        -- hieH _ _ = return (IdeResponseOk (String "worked"))
        hieH plugin cmd _ = do
          let
              cid = 1
              reqVal = IdeRequest cmd (Context Nothing Nothing Nothing Nothing) Map.empty
          liftIO $ writeChan cin (CReq plugin cid reqVal cout)
          rsp <- liftIO $ readChan cout
          return (coutResp rsp)

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
jsonHttpListener :: Chan ChannelRequest -> IO ()
jsonHttpListener cin = runTestServer cin 8001
