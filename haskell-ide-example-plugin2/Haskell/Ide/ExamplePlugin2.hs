{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.ExamplePlugin2 where

import Haskell.Ide.Engine.PluginDescriptor

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T

-- ---------------------------------------------------------------------

example2Descriptor :: PluginDescriptor
example2Descriptor = PluginDescriptor
  {
    pdCommands =
      [
        Command
          { cmdDesc = CommandDesc
                       { cmdName = "sayHello"
                       , cmdContexts = [CtxNone]
                       , cmdAdditionalParams = []
                       }
          , cmdFunc = sayHelloCmd
          }
      , Command
          { cmdDesc = CommandDesc
                       { cmdName = "sayHelloTo"
                       , cmdContexts = [CtxNone]
                       , cmdAdditionalParams = [RP "name"]
                       }
          , cmdFunc = sayHelloToCmd
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

sayHelloCmd :: Dispatcher
sayHelloCmd _ = return (IdeResponseOk (String sayHello))

sayHelloToCmd :: Dispatcher
sayHelloToCmd req = do
  case Map.lookup "name" (ideParams req) of
    Nothing -> return $ IdeResponseFail "expecting parameter `name`"
    Just n -> do
      r <- liftIO $ sayHelloTo n
      return $ IdeResponseOk (String r)

-- ---------------------------------------------------------------------
{-
example2Dispatcher :: Dispatcher
example2Dispatcher (IdeRequest name ctx params) = do
  case name of
    "sayHello"   -> return (IdeResponseOk (String sayHello))
    "sayHelloTo" -> do
      case Map.lookup "name" params of
        Nothing -> return $ IdeResponseFail "expecting parameter `name`"
        Just n -> do
          r <- liftIO $ sayHelloTo n
          return $ IdeResponseOk (String r)
-}

-- ---------------------------------------------------------------------

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: String -> IO T.Text
sayHelloTo n = return $ T.pack $ "hello " ++ n ++ " from ExamplePlugin2"
