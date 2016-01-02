{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.ExamplePlugin2 where

import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T

-- ---------------------------------------------------------------------

example2Descriptor :: TaggedPluginDescriptor _
example2Descriptor = PluginDescriptor
  {
    pdUIShortName = "Hello World"
  , pdUIOverview = "An example of writing an HIE plugin"
  , pdCommands =
         buildCommand' sayHelloCmd (Proxy :: Proxy "sayHello") "say hello" [] (SCtxNone :& RNil) RNil
      :& buildCommand' sayHelloToCmd (Proxy :: Proxy "sayHelloTo")
                          "say hello to the passed in param"
                          []
                          (SCtxNone :& RNil)
                          (  SRP (Proxy :: Proxy "name") (Proxy :: Proxy "the name to greet") SPtText
                          :& RNil)

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

sayHelloCmd :: CommandFunc T.Text
sayHelloCmd = CmdSync $ \_ _ -> return (IdeResponseOk sayHello)

sayHelloToCmd :: CommandFunc T.Text
sayHelloToCmd = CmdSync $ \_ req -> do
  case Map.lookup "name" (ideParams req) of
    Nothing -> return $ missingParameter "name"
    Just (ParamTextP n) -> do
      r <- liftIO $ sayHelloTo n
      return $ IdeResponseOk r
    Just x -> return $ incorrectParameter "name" ("ParamText"::String) x

-- ---------------------------------------------------------------------
{-
example2CommandFunc :: CommandFunc
example2CommandFunc (IdeRequest name ctx params) = do
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

sayHelloTo :: T.Text -> IO T.Text
sayHelloTo n = return $ "hello " <> n <> " from ExamplePlugin2"
