{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.HaRePlugin where

import Haskell.Ide.Engine.PluginDescriptor

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T

-- ---------------------------------------------------------------------

hareDescriptor :: PluginDescriptor
hareDescriptor = PluginDescriptor
  {
    pdUiCommands =
      [
        UiCommand
          { uiDesc = UiCommandDesc
                     { uiCmdName = "sayHello"
                     , uiContexts = [CtxNone]
                     , uiAdditionalParams = []
                     }
          , uiFunc = sayHelloCmd
          }
      , UiCommand
          { uiDesc = UiCommandDesc
                       { uiCmdName = "sayHelloTo"
                       , uiContexts = [CtxNone]
                       , uiAdditionalParams = [RP "name"]
                       }
          , uiFunc = sayHelloToCmd
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

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: String -> IO T.Text
sayHelloTo n = return $ T.pack $ "hello " ++ n ++ " from ExamplePlugin2"
