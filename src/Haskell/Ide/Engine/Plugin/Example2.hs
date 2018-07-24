{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Example2 where

import           Control.Monad.IO.Class
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                     as T
import           Haskell.Ide.Engine.MonadTypes

-- ---------------------------------------------------------------------

example2Descriptor :: PluginDescriptor
example2Descriptor = PluginDescriptor
  {
    pluginName = "Hello World"
  , pluginDesc = "An example of writing an HIE plugin"
  , pluginCommands =
      [ PluginCommand "sayHello" "say hello" sayHelloCmd
      , PluginCommand "sayHelloTo ""say hello to the passed in param" sayHelloToCmd
      ]
  , pluginCodeActions = noCodeActions
  }

-- ---------------------------------------------------------------------

sayHelloCmd :: CommandFunc () T.Text
sayHelloCmd = CmdSync $ \_ -> return (IdeResultOk sayHello)

sayHelloToCmd :: CommandFunc T.Text T.Text
sayHelloToCmd = CmdSync $ \n -> do
  r <- liftIO $ sayHelloTo n
  return $ IdeResultOk r

-- ---------------------------------------------------------------------

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: T.Text -> IO T.Text
sayHelloTo n = return $ "hello " <> n <> " from ExamplePlugin2"
