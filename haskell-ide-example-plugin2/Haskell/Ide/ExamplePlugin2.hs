{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.ExamplePlugin2 where

import Haskell.Ide.PluginDescriptor

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T

-- ---------------------------------------------------------------------

example2Descriptor :: PluginDescriptor
example2Descriptor = PluginDescriptor
  {
    pdUiCommands =
      [
        UiCommand
          { uiCmdName = "sayHello"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          }
      , UiCommand
          { uiCmdName = "sayHelloTo"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = [RP "name"]
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

example2Dispatcher :: Dispatcher
example2Dispatcher (IdeRequest name session ctx params) = do
  case name of
    "sayHello"   -> return (IdeResponseOk (String sayHello))
    "sayHelloTo" -> do
      case Map.lookup "name" params of
        Nothing -> return $ IdeResponseFail "expecting parameter `name`"
        Just n -> do
          r <- sayHelloTo n
          return $ IdeResponseOk (String r)

-- ---------------------------------------------------------------------

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: String -> IO T.Text
sayHelloTo n = return $ T.pack $ "hello " ++ n ++ " from ExamplePlugin2"
