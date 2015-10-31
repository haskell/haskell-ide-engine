module Haskell.Ide.ExamplePlugin2 where

import Haskell.Ide.PluginDescriptor

import qualified Data.Map as Map

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
    "sayHello"   -> return (IdeResponseOk sayHello)
    "sayHelloTo" -> do
      case Map.lookup "name" params of
        Nothing -> return $ IdeResponseFail "expecting parameter `name`"
        Just n -> do
          r <- sayHelloTo n
          return $ IdeResponseOk r

-- ---------------------------------------------------------------------

sayHello :: String
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: String -> IO String
sayHelloTo n = return $ "hello " ++ n ++ " from ExamplePlugin2"
